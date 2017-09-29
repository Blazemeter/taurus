#!/usr/bin/env bash
#
# first time run can require sudo (to create /home/linuxbrew/.linuxbrew)
# requirements: (apt)
#       ruby for linuxbrew
#       gcc-multilib for build psuitl, etc.; see
#           https://stackoverflow.com/questions/6329887/compiling-problems-cannot-find-crt1-o)
#
echo -n "Clean build directory... "
BUILD_DIR="$(dirname $0)/build/brew"
rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"
echo "done"

FORMULA="${BUILD_DIR}/bzt.rb"

PLATFORM=`uname`
if [ "$PLATFORM" = "Linux" ]; then
    BREW_LINK="https://raw.githubusercontent.com/Linuxbrew/install/master"
    GLOBAL_BREW="/home/linuxbrew/.linuxbrew:$HOME/.linuxbrew"
elif [ "$PLATFORM" = "Darwin" ]; then
    BREW_LINK="https://raw.githubusercontent.com/Homebrew/install/master"
    GLOBAL_BREW="/usr/local"
else
    echo "Wrong build platform: $PLATFORM"
    exit 1
fi

PATH="${GLOBAL_BREW}/bin":"${PATH}"

# If brew isn't found install it. This link for linux only!
command -v brew >/dev/null 2>&1 ||
    echo | ruby -e "$(curl -fsSL ${BREW_LINK}/install)"
    # suppress interactive mode (ENTER for confirmation)

brew remove --force --ignore-dependencies $(brew list)
brew install --force-bottle python2

if [ -z "$1" ]; then
    BZT_VER="1.9.6"
else
    BZT_VER="$1"
fi

PYPKG_URL="https://files.pythonhosted.org/packages/source/b/bzt/bzt-${BZT_VER}.tar.gz"
SHA256=`curl -L -s "${PYPKG_URL}" | shasum -a 256 | awk '{split($0, a); print a[1]}'`

pip2 install virtualenv

# write header to formula
cat << EOF > "${FORMULA}"
class Bzt < Formula
  include Language::Python::Virtualenv
  desc "BlazeMeter Taurus"
  homepage "https://gettaurus.org"
  url "${PYPKG_URL}"
  sha256 "${SHA256}"
  head "https://github.com/greyfenrir/taurus.git"
  depends_on :python if MacOS.version <= :snow_leopard

EOF

# Set up a temporary virtual environment
virtualenv ${BUILD_DIR}/venv -p python2
source ${BUILD_DIR}/venv/bin/activate

# Install the package of interest as well as homebrew-pypi-poet
pip install bzt homebrew-pypi-poet

# Get stanzas
poet bzt >> "${FORMULA}"

# Destroy the temporary virtualenv you just created
deactivate

# add footer of formula
cat << EOF >> "${FORMULA}"

  def install
    virtualenv_install_with_resources
    bin.install_symlink "#{libexec}/bin/bzt" => "bzt"
    bin.install_symlink "#{libexec}/bin/jmx2yaml" => "jmx2yaml"
    bin.install_symlink "#{libexec}/bin/soapui2yaml" => "soapui2yaml"
  end

  test do
    system "#{bin}/bzt", "--help"
  end
end
EOF

#if [ "${PLATFORM}" = "Linux" ]; then
#    BREW_FORMULA="$(brew --prefix)/opt/bzt/.brew/bzt.rb"
#else
#    BREW_FORMULA="$(brew --prefix)/Homebrew/Library/Taps/homebrew/homebrew-core/Formula/bzt.rb"
#fi

PREFIX=$(brew --prefix)

brew install --build-from-source "${FORMULA}" &&
    chmod 644 `find $PREFIX -name bzt.rb` &&   # brew audit requires such access rights
    brew test bzt &&
    brew audit --strict --online bzt

# next steps:
#  1. fork the Homebrew/homebrew-core
#  2. add remote rep to fork
#  3. add formula, create branch and commit
#  4. push to rep, make PR
#  (see https://docs.brew.sh/How-To-Open-a-Homebrew-Pull-Request.html)
#
# use 'brew bump-formula-pr' for existing formula

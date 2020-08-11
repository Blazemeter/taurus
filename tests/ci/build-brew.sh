#!/usr/bin/env bash
#
# first time run requires sudo (to create brew dirs)
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
echo "Build platform detected: ${PLATFORM}"
if [ "$PLATFORM" = "Linux" ]; then
    BREW_LINK="https://raw.githubusercontent.com/Linuxbrew/install/master"
    BREW_BIN="/home/linuxbrew/.linuxbrew/bin:$HOME/.linuxbrew/bin"
elif [ "$PLATFORM" = "Darwin" ]; then
    BREW_LINK="https://raw.githubusercontent.com/Homebrew/install/master"
    BREW_BIN="/usr/local/bin"
else
    echo "Wrong build platform: $PLATFORM"
    exit 1
fi

PATH="${BREW_BIN}:${PATH}"

# If brew isn't found install it. This link for linux only!
command -v brew >/dev/null 2>&1 ||
    echo | ruby -e "$(curl -fsSL ${BREW_LINK}/install)"
    # suppress interactive mode (ENTER for confirmation)

BREW_LIST="`brew list`"
if [ ! -z "$BREW_LIST" ]; then
  brew remove --force --ignore-dependencies $(brew list)
fi

brew update
brew install python

if [ -z "$1" ]; then
    BZT_VER="1.9.6"
else
    BZT_VER="$1"
fi
PYPKG_URL="https://files.pythonhosted.org/packages/source/b/bzt/bzt-${BZT_VER}.tar.gz"
SHA256=`curl -L -s "${PYPKG_URL}" | shasum -a 256 | awk '{split($0, a); print a[1]}'`

pip3 uninstall virtualenv -y
pip3 install virtualenv

# write header to formula
cat << EOF > "${FORMULA}"
class Bzt < Formula
  include Language::Python::Virtualenv
  desc "BlazeMeter Taurus"
  homepage "https://gettaurus.org"
  url "${PYPKG_URL}"
  sha256 "${SHA256}"
  head "https://github.com/Blazemeter/taurus.git"

  depends_on "python"

EOF

# Set up a temporary virtual environment
python3 -m virtualenv ${BUILD_DIR}/venv -p python3
source ${BUILD_DIR}/venv/bin/activate
# Install the package of interest as well as homebrew-pypi-poet
pip3 install bzt homebrew-pypi-poet

# Get stanzas
poet bzt >> "${FORMULA}"

# Destroy the temporary virtualenv you just created
deactivate

# add footer of formula
cat << EOF >> "${FORMULA}"

  def install
    # Fix "ld: file not found: /usr/lib/system/libsystem_darwin.dylib" for lxml
    ENV["SDKROOT"] = MacOS.sdk_path if MacOS.version == :sierra

    virtualenv_install_with_resources
  end

  test do
    cmd = "#{bin}/bzt -v -o execution.executor=apiritif -o execution.iterations=1 -o execution.scenario.requests.0=https://gettaurus.org/"
    # assert_match /INFO: Samples count: 1, .*% failures/, shell_output(cmd)
    system(cmd)
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
    brew audit --strict --online bzt &&
        echo ">>>>>>>> start of formula <<<<<<<<" &&
        cat ${FORMULA} &&
        echo ">>>>>>>> end of formula <<<<<<<<"

# steps to first deploy:
#  1. fork the Homebrew/homebrew-core
#  2. clone the fork to local machine
#  3. run this script
#  4. add formula from build/brew/bzt.rb to home-core/Formula
#  5. create branch `bzt` and commit 'bzt <version>'
#  6. push to fork, make PR
#  (see https://docs.brew.sh/How-To-Open-a-Homebrew-Pull-Request.html)
#
# steps to upgrade:
# 1. go to master branch, remove others (include origin/bzt).
# 2. update repo from <homebrew>/master
# 4. run script and provide new bzt version as param (e.q. './build-brew.sh 2.2.2')
# 5. use follow: source=build/brew/bzt.rb destination=home-core/Formula/bzt.rb
# 6. copy all 'resource's except of bzt itself (it's poet mistake)
# 7. update url and sha256 of `class Bzt`
# 8. don't touch anything else!
# 9. create branch `bzt` and commit 'bzt <version>'
# 10. push to origin repo, make PR
#
# Looks like 'brew bump-formula-pr' can handle only trivial cases.


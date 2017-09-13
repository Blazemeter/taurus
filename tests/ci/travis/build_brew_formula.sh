#!/bin/bash

PYPKG_URL=https://files.pythonhosted.org/packages/4f/85/3730bf6788e9d22a430324fef9dc81b7b65718ab1d6e2485d1eb12fc8d4f/bzt-1.9.5.tar.gz
SHA256=`curl -L -s "${PYPKG_URL}" | shasum -a 256 | awk '{split($0, a); print a[1]}'`

BUILD_DIR="$(dirname $0)/build/brew"
mkdir -p "$BUILD_DIR"

FORMULA_FILE="${BUILD_DIR}/bzt.rb"
# LOCAL_BREW="$HOME/.linuxbrew"
# GLOBAL_BREW="/home/linuxbrew/.linuxbrew"

# todo: return path/install order back
# PATH="${LOCAL_BREW}"/bin:"${GLOBAL_BREW}"/bin:"${PATH}"

# If brew isn't found install it
# if ! command -v brew >/dev/null 2>&1; then
#   # suppress interactive mode (ENTER for confirmation)
#   echo | ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install)"
# fi

BREW_FORMULA="$(brew --prefix)/Homebrew/Library/Taps/homebrew/homebrew-core/Formula/bzt.rb"

# write header to formula
cat << EOF > "${FORMULA_FILE}"
class Bzt < Formula
  include Language::Python::Virtualenv
  desc "BlazeMeter Taurus"
  homepage "https://gettaurus.org"
  url "${PYPKG_URL}"
  sha256 "${SHA256}"
  head "https://github.com/greyfenrir/taurus.git"
  depends_on :python

EOF

brew install python libxml2

# Set up a temporary virtual environment
virtualenv --clear $BUILD_DIR/venv -p python
source $BUILD_DIR/venv/bin/activate
# Install the package of interest as well as homebrew-pypi-poet
pip install bzt homebrew-pypi-poet

# Get stanzas
poet bzt >> "${FORMULA_FILE}"

# Destroy the temporary virtualenv you just created
deactivate

# add footer of formula
cat << EOF >> "${FORMULA_FILE}"

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

ln -sf "${FORMULA_FILE}" "${BREW_FORMULA}"
chmod 644 "${FORMULA_FILE}"

brew update
brew reinstall bzt -vvv
brew test bzt
brew audit --strict --online bzt

echo ">>>>> start of formula"
cat ${FORMULA_FILE}
echo ">>>>> end of formula"
# todo:
#  1. fork the Homebrew/homebrew-core
#  2. add remote rep to fork
#  3. add formula, create branch and commit
#  4. push to rep, make PR
#  (see https://docs.brew.sh/How-To-Open-a-Homebrew-Pull-Request.html)
#
# use 'brew bump-formula-pr' for existing formula

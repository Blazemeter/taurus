#!/usr/bin/env bash

PYPKG_URL=https://files.pythonhosted.org/packages/source/b/bzt/bzt-1.9.5.tar.gz
SHA256=`curl -L -s "${PYPKG_URL}" | shasum -a 256 | awk '{split($0, a); print a[1]}'`

BUILD_DIR=`readlink -f "$(dirname $0)/build/brew"`
rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

FORMULA="${BUILD_DIR}/bzt.rb"
LOCAL_BREW="$HOME/.linuxbrew"
GLOBAL_BREW="/home/linuxbrew/.linuxbrew"

PATH="${LOCAL_BREW}"/bin:"${GLOBAL_BREW}"/bin:"${PATH}"

# If brew isn't found install it. This link for linux only!
command -v brew >/dev/null 2>&1 || (
  echo | ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install)")
  # suppress interactive mode (ENTER for confirmation)

brew install python

# write header to formula
cat << EOF > "${FORMULA}"
class Bzt < Formula
  include Language::Python::Virtualenv
  desc "BlazeMeter Taurus"
  homepage "https://gettaurus.org"
  url "${PYPKG_URL}"
  sha256 "${SHA256}"
  head "https://github.com/greyfenrir/taurus.git"
  depends_on :python
  depends_on "libxml2" => :build

EOF

# Set up a temporary virtual environment
virtualenv --clear ${BUILD_DIR}/venv -p python
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


chmod 644 "${FORMULA}"

virtualenv --clear ${BUILD_DIR}/venv -p python
source ${BUILD_DIR}/venv/bin/activate
brew update
brew reinstall --build-from-source "${FORMULA}" -vvv

brew test bzt
brew audit --strict --online bzt
brew deactivate

# todo:
#  1. fork the Homebrew/homebrew-core
#  2. add remote rep to fork
#  3. add formula, create branch and commit
#  4. push to rep, make PR
#  (see https://docs.brew.sh/How-To-Open-a-Homebrew-Pull-Request.html)
#
# use 'brew bump-formula-pr' for existing formula
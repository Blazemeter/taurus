#!/bin/bash

PYPKG_URL=https://files.pythonhosted.org/packages/4f/85/3730bf6788e9d22a430324fef9dc81b7b65718ab1d6e2485d1eb12fc8d4f/bzt-1.9.5.tar.gz
BUILD_DIR="$(dirname $0)/build/brew"
FORMULA_FILE="$BUILD_DIR/bzt.rb"
SHA256=`curl -L -s "$PYPKG_URL" | shasum -a 256 | awk '{split($0, a); print a[1]}'`

mkdir -p "$BUILD_DIR"

# Install virtualenvwrapper
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install)"
test -d ~/.linuxbrew && PATH="$HOME/.linuxbrew/bin:$PATH"
test -d /home/linuxbrew/.linuxbrew && PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"

# write header to formula
cat << EOF > "$FORMULA_FILE"
class Bzt < Formula
  include Language::Python::Virtualenv
  desc "BlazeMeter Taurus"
  homepage "https://gettaurus.org"
  url "\$PYPKG_URL"
  sha256 "\$SHA256"
  head "https://github.com/greyfenrir/taurus.git"
  depends_on :python3
EOF

brew install python3
python3 -m pip install virtualenvwrapper
source virtualenvwrapper.sh

# Set up a temporary virtual environment
mktmpenv

# Install the package of interest as well as homebrew-pypi-poet
pip install bzt homebrew-pypi-poet

# Get stanzas
poet bzt >> "$FORMULA_FILE"

# Destroy the temporary virtualenv you just created
deactivate

# add footer of formula
cat << EOF >> "$FORMULA_FILE"
  def install
    virtualenv_install_with_resources
    bin.install_symlink "#{libexec}/bin/bzt" => "bzt"
    bin.install_symlink "#{libexec}/bin/jmx2yaml" => "jmx2yaml"
    bin.install_symlink "#{libexec}/bin/soapui2yaml" => "soapui2yaml"
  end

  test do
    system "bzt", "--help"
  end
end
EOF

ln -s "$FORMULA_FILE" "$(brew --prefix)/Homebrew/Library/Taps/homebrew/homebrew-core/Formula/bzt.rb"

brew test
brew audit --strict --online bzt

# todo:
#  1. fork the Homebrew/homebrew-core
#  2. add remote rep to fork
#  3. add formula, create branch and commit
#  4. push to rep, make PR
#  (see https://docs.brew.sh/How-To-Open-a-Homebrew-Pull-Request.html)
#
# use 'brew bump-formula-pr' for existing formula

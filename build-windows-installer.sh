#!/bin/bash
set -euo pipefail

BUILD_DIR="$(dirname $0)/build/nsis"

# clean up build dir
rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

# ensure that argument was given
if [ "$#" -ne 1 ]
then
  echo "Usage: $0 <build_file>"
  exit 1
fi
if ! [ -e "$1" ]; then
  echo "$1 not found" >&2
  exit 1
fi

TAURUS_DIST="$1"

# install pythonz
curl -kL https://raw.github.com/saghul/pythonz/master/pythonz-install | bash

PYTHONZ_HOME="${HOME}/.pythonz/bin/pythonz"
PYTHONZ_SYSTEM="/usr/local/pythonz/bin/pythonz"
[ -f "$PYTHONZ_HOME" ] && PYTHONZ="$PYTHONZ_HOME" || PYTHONZ="$PYTHONZ_SYSTEM"

# install specific version of python
$PYTHONZ install 3.6.3

# set up python virtualenv
virtualenv venv --python=$($PYTHONZ locate 3.6.3)

# this is a workaround for venv bug: https://github.com/pypa/virtualenv/issues/1029
set +u
source venv/bin/activate
set -u

# setup packages needed for build
python -m pip install -U pip-custom-platform wheel
python -m pip install pynsist==2.1
python scripts/installer/gen_installer.py "$TAURUS_DIST"

# deactivate venv afterwards
deactivate
# Installer was saved to ${BUILD_DIR}/TaurusInstaller_${TAURUS_VERSION}_x64.exe

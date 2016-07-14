#!/bin/bash
set -euo pipefail

TAURUS_VERSION=$(python -c 'import bzt; print(bzt.VERSION)')
INSTALLER_NAME="TaurusInstaller_${TAURUS_VERSION}_x64.exe"
BUILD_DIR="$(dirname $0)/build/nsis"

rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

# create NSIS script
cat << EOF > "$BUILD_DIR/taurus.nsi"
[% extends "pyapp_w_pylauncher.nsi" %]

[% block install_commands %]
[[ super() ]]
  nsExec::ExecToLog 'py -m pip install --upgrade pip==8.1.2'
  nsExec::ExecToLog 'py -m pip install bzt>=${TAURUS_VERSION}'
[% endblock %]

[% block uninstall_commands %]
[[ super() ]]
  nsExec::ExecToLog 'py -m pip uninstall -y bzt'
[% endblock %]
EOF

cat << EOF > /tmp/fakerunner.py
from bzt import cli
cli.main()
EOF

# Create pynsist config
cat << EOF > "$BUILD_DIR/installer.cfg"
[Application]
name=Taurus
version=${TAURUS_VERSION}
# entry_point=bzt.cli:main
script=/tmp/fakerunner.py
console=true

[Command bzt]
entry_point=bzt.cli:main

[Command jmx2yaml]
entry_point=bzt.jmx2yaml:main

[Python]
version=2.7.12
bitness=64

[Build]
nsi_template=taurus.nsi
directory=${BUILD_DIR}
installer_name=${INSTALLER_NAME}
EOF

pynsist "$BUILD_DIR/installer.cfg"
# Installer was saved to ${BUILD_DIR}/${INSTALLER_NAME}

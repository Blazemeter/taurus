#!/bin/bash
set -euo pipefail

TAURUS_DIST="$1"
TAURUS_DIST_BASENAME=$(basename "$TAURUS_DIST")
TAURUS_DIST_VERSION=$(echo -n "$TAURUS_DIST_BASENAME" | python -c 'import re, sys; print re.match(r"bzt\-([\d\.]+)\.tar\.gz", sys.stdin.read()).group(1)')
BUILD_DIR=$(realpath build/nsis)

rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

# create NSIS script
cat << EOF > "$BUILD_DIR/taurus.nsi"
[% extends "pyapp_w_pylauncher.nsi" %]

[% block install_commands %]
[[ super() ]]
  nsExec::ExecToLog 'py -m pip install --upgrade pip'
  nsExec::ExecToLog 'py -m pip install "\$INSTDIR\\${TAURUS_DIST_BASENAME}"'
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
version=${TAURUS_DIST_VERSION}
# entry_point=bzt.cli:main
script=/tmp/fakerunner.py
console=true

[Command bzt]
entry_point=bzt.cli:main

[Command jmx2yaml]
entry_point=bzt.jmx2yaml:main

[Python]
version=2.7.12
bitness=32

[Include]
files=$(realpath "$TAURUS_DIST")

[Build]
nsi_template=taurus.nsi
directory=${BUILD_DIR}
EOF

pynsist "$BUILD_DIR/installer.cfg"

#!/bin/bash

TAURUS_VERSION=`python -c 'import bzt; print(bzt.VERSION)'`

# setup virtualenv
virtualenv --clear build
source build/bin/activate
pip install --upgrade pynsist

rm -rf build/nsis
mkdir -p build/nsis

# create NSIS script
cat << EOF > taurus.nsi
[% extends "pyapp_w_pylauncher.nsi" %]

[% block install_commands %]
[[ super() ]]
  nsExec::ExecToLog 'py -m pip install --upgrade pip'
  nsExec::ExecToLog 'py -m pip install --upgrade bzt==${TAURUS_VERSION}'

[% endblock %]

EOF

# Create pynsist config
cat << EOF > installer.cfg
[Application]
name=Taurus
version=${TAURUS_VERSION}
# entry_point=bzt.cli:main
script=scripts/bztrunner.py
console=true

[Command bzt]
entry_point=bzt.cli:main

[Command jmx2yaml]
entry_point=bzt.jmx2yaml:main

[Python]
version=2.7.12
bitness=32

[Build]
nsi_template=taurus.nsi
directory=build/nsis
EOF

pynsist installer.cfg

deactivate
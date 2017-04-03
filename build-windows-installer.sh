#!/bin/bash
set -euo pipefail

TAURUS_VERSION=$(python -c 'import bzt; print(bzt.VERSION)')
INSTALLER_NAME="TaurusInstaller_${TAURUS_VERSION}_x64.exe"
BUILD_DIR="$(dirname $0)/build/nsis"
ICON_RELPATH="../../site/img/taurus.ico"  # must have this path relative to the build dir

rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

# create NSIS script
cat << EOF > "$BUILD_DIR/taurus.nsi"
[% extends "pyapp_w_pylauncher.nsi" %]

[% block install_commands %]
[[ super() ]]
  ; Install fresh pip
  nsExec::ExecToLog 'py -m pip install --upgrade pip>=8.1.2'
  Pop \$0
  IntCmp \$0 0 InstalledPip CantInstallPip CantInstallPip

InstalledPip:

  ; Install Taurus
  nsExec::ExecToLog 'py -m pip install bzt>=${TAURUS_VERSION}'
  Pop \$0
  IntCmp \$0 0 InstalledBzt CantInstallBzt CantInstallBzt

InstalledBzt:
  Goto EndInstall

CantInstallPip:
  DetailPrint "Error: can't install pip"
  Abort
  Goto EndInstall

CantInstallBzt:
  DetailPrint "Error: can't install Taurus"
  Abort
  Goto EndInstall

EndInstall:
[% endblock %]

[% block uninstall_commands %]
[[ super() ]]
  nsExec::ExecToLog 'py -m pip uninstall -y bzt'
[% endblock %]

[% block uninstall_files %]
[[ super() ]]
  ; Remove installation log
  Delete "\$INSTDIR\install.log"
[% endblock %]

[% block sections %]
[[ super() ]]

Section
  ; Dump installation log
  StrCpy \$0 "\$INSTDIR\install.log"
  Push \$0
  Call DumpLog
SectionEnd

!define LVM_GETITEMCOUNT 0x1004
!define LVM_GETITEMTEXT 0x102D

Function DumpLog
  Exch \$5
  Push \$0
  Push \$1
  Push \$2
  Push \$3
  Push \$4
  Push \$6
  FindWindow \$0 "#32770" "" \$HWNDPARENT
  GetDlgItem \$0 \$0 1016
  StrCmp \$0 0 exit
  FileOpen \$5 \$5 "w"
  StrCmp \$5 "" exit
    SendMessage \$0 \${LVM_GETITEMCOUNT} 0 0 \$6
    System::Alloc \${NSIS_MAX_STRLEN}
    Pop \$3
    StrCpy \$2 0
    System::Call "*(i, i, i, i, i, i, i, i, i) i \
      (0, 0, 0, 0, 0, r3, \${NSIS_MAX_STRLEN}) .r1"
    loop: StrCmp \$2 \$6 done
      System::Call "User32::SendMessageA(i, i, i, i) i \
        (\$0, \${LVM_GETITEMTEXT}, \$2, r1)"
      System::Call "*\$3(&t\${NSIS_MAX_STRLEN} .r4)"
      FileWrite \$5 "\$4$\r\$\n"
      IntOp \$2 \$2 + 1
      Goto loop
    done:
      FileClose \$5
      System::Free \$1
      System::Free \$3
  exit:
    Pop \$6
    Pop \$4
    Pop \$3
    Pop \$2
    Pop \$1
    Pop \$0
    Exch \$5
FunctionEnd

[% endblock %]
EOF

cat << EOF > "$BUILD_DIR/bzt_win.py"
import os, sys

def main():
    sys.exit(os.system("cmd /k bzt --help"))
EOF

# Create pynsist config
cat << EOF > "$BUILD_DIR/installer.cfg"
[Application]
name=Taurus
version=${TAURUS_VERSION}
entry_point=bzt_win:main
console=true
icon=${ICON_RELPATH}

[Command bzt]
entry_point=bzt.cli:main

[Command jmx2yaml]
entry_point=bzt.jmx2yaml:main

[Command soapui2yaml]
entry_point=bzt.soapui2yaml:main

[Python]
version=2.7.12
bitness=64

[Build]
nsi_template=taurus.nsi
directory=.
installer_name=${INSTALLER_NAME}
EOF

pynsist "$BUILD_DIR/installer.cfg"
# Installer was saved to ${BUILD_DIR}/${INSTALLER_NAME}

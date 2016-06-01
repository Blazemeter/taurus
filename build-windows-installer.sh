#!/bin/bash

TAURUS_VERSION=`python -c 'import bzt; print(bzt.VERSION)'`

# setup virtualenv
virtualenv --clear build
source build/bin/activate
pip install --upgrade pynsist
pip install --upgrade colorlog pyyaml psutil lxml cssselect nose urwid coverage six pylint selenium progressbar33 locustio pyvirtualdisplay

rm -rf build/installer
mkdir -p build/installer

cd build/installer

ln -s "../../bzt" bzt

# Download dependency wheels
wget -O lxml.whl https://pypi.python.org/packages/f1/c7/e19d317cc948095abc872a6e6ae78ac80260f2b45771dfa7a7ce86865f5b/lxml-3.6.0-cp27-none-win32.whl#md5=b2f7e0c83e48e334fcd6b5b3412f18c4
wget -O psutil.whl https://pypi.python.org/packages/58/a5/2ccc9f6180ea769005405381f6b0d01fe1268f20cc85877b02c04c27d306/psutil-4.2.0-cp27-cp27m-win32.whl#md5=0caae8cea2d0ffe3b6ce6d2b25a9271a
wget -O selenium.whl https://pypi.python.org/packages/1e/27/47f73510c6b80d1ff0829474947537ae9ab8d516cc48c6320b7f3677fa54/selenium-2.53.2-py2.py3-none-any.whl#md5=81ce6cbf3ed030bdaf921fae58024a4f
wget -O urwid.tar.gz https://pypi.python.org/packages/source/u/urwid/urwid-1.3.1.tar.gz

mkdir pynsist_pkgs

# Unzip the bindings
7z x psutil.whl -opsutil
7z x lxml.whl -olxml
7z x selenium.whl -oselenium
tar -xf urwid.tar.gz

cp -r lxml/lxml pynsist_pkgs
cp -r psutil/psutil pynsist_pkgs
cp -r selenium/selenium pynsist_pkgs
cp -r urwid-1.3.1/urwid pynsist_pkgs

# create NSIS script
cat << EOF > taurus.nsi
[% extends "pyapp_w_pylauncher.nsi" %]

[% block install_commands %]
[[ super() ]]
  IfFileExists "C:\etc\bzt.d\99-installID.yml" file_found file_not_found
  file_not_found:
  nsExec::ExecToStack '[[ python ]] -c "import uuid; print uuid.uuid4()"'
  Pop \$0  ; return code
  Pop \$1  ; output
  FileOpen \$9 C:\etc\bzt.d\99-installID.yml w
  FileWrite \$9 "---\$\r\$\n"
  FileWrite \$9 "install-id: "
  FileWrite \$9 \$1
  FileWrite \$9 "\$\r\$\n"
  FileClose \$9
  file_found:

[% endblock %]

EOF

# Create pynsist config
cat << EOF > installer.cfg
[Application]
name=Taurus
version=${TAURUS_VERSION}
entry_point=bzt.cli:main
console=true

[Command bzt]
entry_point=bzt.cli:main

[Command jmx2yaml]
entry_point=bzt.jmx2yaml:main

[Python]
version=2.7.11
bitness=32

[Include]
packages=bzt
    lxml
    psutil
    yaml
    urwid
    selenium
    Tkinter
    colorlog
    colorama
    cssselect
    six
    nose
    progressbar
    pyvirtualdisplay
files=bzt/10-base.json > C:\etc\bzt.d\

[Build]
nsi_template=taurus.nsi
directory=.

EOF

pynsist installer.cfg || exit 1

deactivate

echo "Taurus installer was saved as build/installer/Taurus_$TAURUS_VERSION.exe"

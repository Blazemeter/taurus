#!/bin/bash -xe

# setup env
virtualenv --clear --system-site-packages build

mv build/bin/pip build/bin/real_pip
printf '%s\n"' '#!/bin/bash' > build/bin/pip
python -c 'import os; import sys; sys.stdout.write(os.path.abspath(os.path.join(os.curdir, "build/bin/python")))' >> build/bin/pip
printf '" "'  >> build/bin/pip
python -c 'import os; import sys; sys.stdout.write(os.path.abspath(os.path.join(os.curdir, "build/bin/real_pip")))' >> build/bin/pip
printf '" $@' >> build/bin/pip
chmod +x build/bin/pip

source build/bin/activate

# install depends
pip install colorlog pyyaml psutil lxml cssselect grip nose urwid coverage six

# run unit tests
./run-test.sh

# run installation test
pip -v install dist/bzt-*.tar.gz

# run functional tests
bzt -o execution.scenario.script=tests/jmx/dummy.jmx -d build/test -o modules.jmeter.path=build/jmeter/bin/jmeter
bzt -d build/test tests/json/jmeter_minimal.json -o modules.jmeter.path=build/jmeter/bin/jmeter
bzt -d build/test tests/json/get-post.json -o modules.jmeter.path=build/jmeter/bin/jmeter

# generate docs
./make-docs.sh

deactivate

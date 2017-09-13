#!/bin/sh -e

mono --version
nvm install 4.2
nvm use 4.2
gem install rspec
pip install colorlog jsonpath-rw pyyaml psutil lxml cssselect urwid six selenium progressbar33 locustio pyvirtualdisplay astunparse ipaddress
pip install git+https://github.com/Blazemeter/apiritif.git@master
pip install --upgrade codecov nose nose-exclude
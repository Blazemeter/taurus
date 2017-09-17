#!/bin/sh -e

mono --version
nvm install 4.2
nvm use 4.2
gem install rspec
pip install pip --upgrade
pip install -r requirements.txt
pip install codecov nose-exclude locustio
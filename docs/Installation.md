# Installing and Upgrading

## Windows

Tried on Windows 7 and Windows XP:

1. Get python 2.7 from http://www.python.org/downloads and install it, don't forget to enable "Add python.exe to Path"
2. Get latest Java from https://www.java.com/download/ and install it
3. Get Microsoft Visual C++ Compiler for Python 2.7 from http://aka.ms/vcpython27 and install it
4. Open Command Prompt, type `pip install bzt` and hit `Enter`

To upgrade it, open Command Prompt and type `pip install --upgrade bzt`, then hit `Enter`

Known limitations on Windows:
 - bzt.log file is not deleted at the end of execution
 - console screen is available only as "text logging"

## Linux
Instructions are tested on Ubuntu 14.04 (Trusty) minimal install, but should work on other distributions with corresponding packages and libs:

Way 1, python dependencies by debian: 

```bash
sudo apt-get install python default-jre-headless python-pip python-lxml python-psutil
sudo pip install bzt
```

Way 2, python dependencies by pip (you have to use it with Ubuntu 12.04): 

```bash
sudo apt-get install python default-jre-headless python-pip python-dev libxml2-dev libxslt-dev zlib1g-dev
sudo pip install bzt
```


## Mac OS

Most likely you need first to upgrade `setuptools`:
```bash
sudo pip install --upgrade setuptools
```

Then install Taurus:
```bash
sudo pip install bzt
```
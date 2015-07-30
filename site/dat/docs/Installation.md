# Installing and Upgrading

## Windows

Tried on Windows 7 and Windows XP:

  1. Get python 2.7 from http://www.python.org/downloads and install it, don't forget to enable "Add python.exe to Path"
  2. Get latest Java from https://www.java.com/download/ and install it
  3. Get Microsoft Visual C++ Compiler for Python 2.7 from http://aka.ms/vcpython27 and install it
  4. Install pip by following these directions: http://pip.readthedocs.org/en/latest/installing.html
  5. Upgrade to the latest setuptools by running the following in a command prompt: `pip install --upgrade setuptools`
  6. Open Command Prompt from administrative account, type `pip install bzt` and hit `Enter`


To upgrade it, open Command Prompt and type `pip install --upgrade bzt`, then hit `Enter`

## Linux
Instructions are tested on Ubuntu 14.04 (Trusty) minimal install, but should work on other distributions with corresponding packages and libs. You will need python 2.7 or higher and Java installed.

```bash
sudo apt-get install python default-jre-headless python-tk python-pip python-dev libxml2-dev libxslt-dev zlib1g-dev
sudo pip install bzt
```

Upgrading to latest is as simple as this:

```bash
sudo pip install --upgrade bzt
```

## Mac OS

You will need python 2.7 or higher and Java Runtime installed.

Most likely you also need first to upgrade `setuptools`:

```bash
sudo pip install --upgrade setuptools
```

Then install Taurus:

```bash
sudo pip install bzt
```

If you experience `libxml/xmlversion.h missing` error, try running:

```bash
brew install libxml2
brew install libxslt
```
then try installing Taurus again.


Upgrading to latest is as simple as this:

```bash
sudo pip install --upgrade bzt
```
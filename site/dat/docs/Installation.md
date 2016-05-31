# Installing and Upgrading

## Windows

The installation process was tried on all supported Windows versions (7, 8, 10).

###  Install Taurus system dependencies

1. Get Python 2.7 from [http://www.python.org/downloads](http://www.python.org/downloads) and install it, don't forget to enable "Add python.exe to Path" checkbox.
1. Get latest Java from [https://www.java.com/download/](https://www.java.com/download/) and install it.

### Install Taurus Python dependencies

Open Command Prompt with administrative privileges (find `Command Prompt` in main menu and chose `Run as administrator`
from context menu). Then run the following command to update Python package manager to the latest version:
```
python -m pip install --upgrade pip
```

#### Install `lxml` package

```
pip install lxml
```

If this command fails, you can install `lxml` with Windows installer provided at project's
[PyPI page](https://pypi.python.org/pypi/lxml/3.6.0). Just download `lxml-3.6.0.win32-py2.7.exe` installer (or
`lxml-3.6.0.win-amd64-py2.7.exe`, if you've installed 64-bit Python) and run it.

#### Install `psutil` package

```
pip install psutil
```

If this command fails, you can install `psutil` with Windows installer provided at project's
[PyPI page](https://pypi.python.org/pypi/psutil). Download `psutil-4.2.0.win32-py2.7.exe` file (or
`psutil-4.2.0.win-amd64-py2.7.exe` for 64-bit Python) and install it.

### Install Taurus

```
pip install bzt
```

### Upgrading Taurus

To upgrade Taurus, open Command Prompt as administrator and run
``` 
 pip install --upgrade bzt
```

## Mac OS

You will need python 2.7 or higher and Java Runtime installed. 

If you're on _El Capitan_, please execute first:

```bash
brew install python
```

Most likely you also need to upgrade `setuptools`:

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

## Linux

### Ubuntu
Instructions are tested on Ubuntu 14.04 (Trusty) minimal install, but should work on other distributions with corresponding packages and libs. You will need python 2.7 or higher and Java installed.

```
sudo apt-get update
sudo apt-get install python default-jre-headless python-tk python-pip python-dev libxml2-dev libxslt-dev zlib1g-dev
sudo pip install bzt
```
Upgrading to latest is as simple as this:

```
sudo pip install --upgrade bzt
```

### CentOS

Use `uname -a` to verify the system if it's 32 bit or 64 bit machine. [http://www.tecmint.com/enable-rpmforge-repository/](http://www.tecmint.com/enable-rpmforge-repository/)

Get corresponding EPEL (Extra Package for Enterprise Linux) for CentOS (Community Enterprise OS) 7, and enable it.

```bash
wget http://dl.fedoraproject.org/pub/epel/7/x86_64/e/epel-release-7-5.noarch.rpm
sudo rpm -ivh epel-release-7-5.noarch.rpm
```

[http://www.tecmint.com/how-to-enable-epel-repository-for-rhel-centos-6-5/](http://www.tecmint.com/how-to-enable-epel-repository-for-rhel-centos-6-5/)

Optional Step: Verify EPEL repository

```bash
sudo yum repolist
```
For example, this has a line like this `epel/x86_64           Extra Packages for Enterprise Linux 7 - x86_64`

Install python-pip

```bash
sudo yum -y install python-pip
```
[http://www.cyberciti.biz/faq/debian-ubuntu-centos-rhel-linux-install-pipclient/](http://www.cyberciti.biz/faq/debian-ubuntu-centos-rhel-linux-install-pipclient/)

Install corresponding libraries for CentOS

```bash
sudo yum install java-1.7.0-openjdk-headless.x86_64 python-devel.x86_64 libxml2-devel.x86_64 libxslt-devel.x86_64 zlib.x86_64 gcc.x86_64
```

You could search by 'yum search xxxxx' where 'xxxxx' represents the name of the library.

Install Taurus:

```bash
sudo pip install bzt
```


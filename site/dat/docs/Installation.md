# Installing and Upgrading

## Windows

Tried on Windows 7 and Windows XP:

  1. Get python 2.7 from [http://www.python.org/downloads](http://www.python.org/downloads) and install it, don't forget to enable "Add python.exe to Path".
  1. Get latest Java from [https://www.java.com/download/](https://www.java.com/download/) and install it.
  1. Download `lxml-3.5.0-cp27-none-win32.whl` package from [http://www.lfd.uci.edu/](http://www.lfd.uci.edu/~gohlke/pythonlibs/#lxml).
  1. Open Command Prompt with administrative privileges (find `Command Prompt` in main menu and chose `Run as administrator` from context menu) and type these commands:
```
  pip install lxml-3.5.0-cp27-none-win32.whl
  pip install --upgrade setuptools
  pip install bzt
```
To upgrade Taurus, open Command Prompt the same way and hit
``` 
 pip install --upgrade bzt
```

## Linux

### Ubuntu
Instructions are tested on Ubuntu 14.04 (Trusty) minimal install, but should work on other distributions with corresponding packages and libs. You will need python 2.7 or higher and Java installed.

```bash
sudo aptitude install python default-jre-headless python-tk python-pip python-dev libxml2-dev libxslt-dev zlib1g-dev
sudo pip install bzt
```

Upgrading to latest is as simple as this:

```bash
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
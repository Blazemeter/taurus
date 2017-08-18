# Installing and Upgrading

## Windows

There are two ways two install Taurus on Windows. The recommended way is to use the prebuilt installer
that will install Python and latest Taurus on your PC. However, if you already have Python installed,
you can install Taurus manually with pip, Python package manager.

The installation process was tried on all supported Windows versions (7, 8, 10).

### Installing Taurus With Prebuilt Installer

Download an [installer](/msi/TaurusInstaller_TAURUS_VERSION_x64.exe) and run it on your system. It will install the following components:
- Python 2.7
- PyLauncher, needed to launch Python programs
- Taurus

Notes:
- Installed Python will be added to PATH as `py` command
- Installed `pip` is not added to PATH, but you can run it with `py -m pip install --upgrade bzt`.

After installation is finished, you can run Taurus with `bzt` from command prompt.

### Installing Taurus Manually

####  Install Taurus System Dependencies

1. Get Python 2.7 from [http://www.python.org/downloads](http://www.python.org/downloads) and install it, don't forget to enable "Add python.exe to Path" checkbox.
1. Get latest Java from [https://www.java.com/download/](https://www.java.com/download/) and install it.

#### Install Taurus Python Dependencies

Open Command Prompt with administrative privileges (find `Command Prompt` in main menu and chose `Run as administrator`
from context menu). Then run the following command to update Python package manager to the latest version:
```
pip install --upgrade pip
```

##### Install `lxml` Package

```
pip install lxml
```

If this command fails, you can install `lxml` with Windows installer provided at project's
[PyPI page](https://pypi.python.org/pypi/lxml/3.6.0). Just download `lxml-3.6.0.win32-py2.7.exe` installer (or
`lxml-3.6.0.win-amd64-py2.7.exe`, if you've installed 64-bit Python) and run it.

##### Install `psutil` Package

```
pip install psutil
```

If this command fails, you can install `psutil` with Windows installer provided at project's
[PyPI page](https://pypi.python.org/pypi/psutil). Download `psutil-4.2.0.win32-py2.7.exe` file (or
`psutil-4.2.0.win-amd64-py2.7.exe` for 64-bit Python) and install it.

##### Install Taurus

```
pip install bzt
```

### Upgrading Taurus

To upgrade Taurus, open Command Prompt as administrator and run
``` 
 pip install --upgrade bzt
```

----

## Mac OS

You will need python 2.7 or higher and Java Runtime installed. 

If you're on _El Capitan_, please execute first:

```bash
brew install python
```

Then try to install Taurus:

```bash
sudo pip install bzt
```

Upgrading Taurus to latest on Mac OS is suggested as:

```bash
sudo pip uninstall bzt && sudo pip install bzt
```

We suggest to avoid using `--upgrade` command with `pip` on Mac OS, since it does not work as expected for Taurus.

### In Case of Failures 
_Try the suggestions below that apply to your case, then repeat `sudo pip install bzt`._

If your Mac OS is older than El Capitan, most likely you also need to upgrade `setuptools`:

```bash
sudo pip install --upgrade setuptools
```

If you experience `libxml/xmlversion.h missing` error, try running:

```bash
brew install libxml2
brew install libxslt
```

----

## Linux

### Ubuntu
Instructions are tested on Ubuntu 14.04 (Trusty) minimal install, but should work on other distributions with corresponding packages and libs. You will need python 2.7 or higher and Java installed.

```
sudo apt-get update
sudo apt-get install python default-jre-headless python-tk python-pip python-dev \
  libxml2-dev libxslt-dev zlib1g-dev
sudo pip install bzt
```
Upgrading to latest is as simple as this:

```
sudo pip install --upgrade bzt
```

Alternatively, you can install using virtualenv, and you will not need sudo privileges for Taurus. Please see [virtualenv](https://virtualenv.pypa.io/en/stable/installation/) instructions for setting up your Python virtual environments, if you haven't done so already.

Then, to install Taurus, type:

```
pip install bzt
```

Upgrade is only:

```
pip install --upgrade bzt
```

### CentOS

Taurus requires Python 2.7 or higher.

Use `uname -a` to verify the system if it's 32 bit or 64 bit machine. [http://www.tecmint.com/enable-rpmforge-repository/](http://www.tecmint.com/enable-rpmforge-repository/)

Get corresponding EPEL (Extra Package for Enterprise Linux) for CentOS (Community Enterprise OS) 7, and enable it.

```bash
wget http://dl.fedoraproject.org/pub/epel/7/x86_64/e/epel-release-7-9.noarch.rpm
sudo rpm -ivh epel-release-7-9.noarch.rpm
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
sudo yum install java-1.7.0-openjdk-headless.x86_64 python-devel.x86_64 \
  libxml2-devel.x86_64 libxslt-devel.x86_64 zlib.x86_64 gcc.x86_64
```

You could search by 'yum search xxxxx' where 'xxxxx' represents the name of the library.

Install Taurus:

```bash
sudo pip install bzt
```


## Docker Image

Taurus has [Docker image](https://hub.docker.com/r/blazemeter/taurus/) that allows you to run tool as container.

To use it, create a directory, for example `/tmp/my-test`, put all configs and additional files like JMXses there, then start Docker like this:

```bash
sudo docker run -i --rm -v /tmp/my-test:/bzt-configs blazemeter/taurus my-config.yml
```

Make note that `/tmp/my-test` was passed in `-v` Docker option, it's crucial. Here's [what happens](https://github.com/Blazemeter/taurus/blob/master/Dockerfile) inside the container:
 1. Directory `/tmp/my-test` is mounted as `/bzt-configs`
 1. Current directory changed to `/bzt-configs`
 1. Taurus is started with the config files you specified: `bzt /bzt-configs/my-config.yml

You can also specify multile config files in the `docker run` command with wildcards or as separate arguments like so:

```bash
sudo docker run -i --rm -v /tmp/my-test:/bzt-configs undera/taurus *.yml

sudo docker run -i --rm -v /tmp/my-test:/bzt-configs undera/taurus my-config-1.json my-config-2.json
```

### Additional Taurus Command-Line Options

You can still pass [command-line options](https://github.com/Blazemeter/taurus/blob/master/site/dat/docs/CommandLine.md) to Taurus through the Docker image. To do so, add the command line option at the end of the `docker run` command like so:

```bash
sudo docker run -i --rm -v /tmp/my-test:/bzt-configs undera/taurus my-config-1.yml -o scenarios.sample.data-sources.0=data.csv
```


### Accessing Taurus Artifacts
If you want to receive Taurus artifacts from container, just mount some directory as `/tmp/artifacts` and files will get there. Following example gives you artifacts in `/tmp/my-run-artifacts` directory.

```bash
sudo docker run -i --rm -v /tmp:/bzt-configs -v /tmp/my-run-artifacts:/tmp/artifacts undera/taurus
```

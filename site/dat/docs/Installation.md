# Installing and Upgrading

## Windows

There are two ways two install Taurus on Windows. The recommended way is to use the prebuilt installer
that will install latest Taurus on your PC. However, if you already have Python installed,
you can install Taurus manually with pip, Python package manager.

The installation process was tried on all supported Windows versions (7, 8, 10).

### Installing Taurus With Prebuilt Installer

<div class="alert alert-danger">
Taurus is changing from network installation scheme to self-contained installers with 1.12.2.

It is highly recommended to uninstall previously installed Taurus through the 'Add or Remove programs' interface
before installing Taurus 1.12.2+.
</div>

Download an [installer](RELEASE_SNAPSHOT) and run it on your system. It will install
local Python 3.6 and Taurus with all its dependencies.

Notes:
- You can use `/S` option of the installer to perform silent installation.

After the installation is finished, you can run Taurus with `bzt` from command prompt. Also, you'll have 'Taurus'
shortcut in the Start menu.

If you want to install additional Python packages along with Taurus (for example Locust, Molotov or others),
you should utilize the following command line utilities provided by the installer:
- `bzt-pip` is a wrapper for pip that can be used to install packages, for example `bzt-pip install locust`
- `bzt-run` can be used to launch packages that were installed with `bzt-pip`, for example `bzt-run locust locustfile.py`

Notes:
- You need to install fresh `setuptools` and `wheel` before installing everything else with `bzt-pip`, so the command is
  `bzt-pip install setuptools wheel`.

### Installing Taurus Manually

####  Install Taurus System Dependencies

1. Get Python 2.7 or 3.5+ from [http://www.python.org/downloads](http://www.python.org/downloads) and install it, don't forget to enable "Add python.exe to Path" checkbox.
1. Get latest Java from [https://www.java.com/download/](https://www.java.com/download/) and install it.

#### Install Taurus Python Dependencies

Open Command Prompt with administrative privileges (find `Command Prompt` in main menu and chose `Run as administrator`
from context menu). Then run the following command to update Python package manager to the latest version:
```
pip install --upgrade pip wheel
```

#### Install Taurus

```
pip install bzt
```

This should install all Taurus dependencies and Taurus itself. You may have to execute this command as an administrator,
if that's how your Python installation handles Python packages.

### Upgrading Taurus

To upgrade Taurus, open Command Prompt as administrator and run
```
pip install --upgrade bzt
```

----

## Mac OS
### Install Homebrew Package
You can use [brew](https://brew.sh/) package manager to install taurus:
```bash
brew install bzt
```
and to update it:
```bash
brew upgrade bzt
```
If your brew auto update is switched off don't forget to manage it manually.
Keep in mind: some additional software can be required depend of test type
([JVM/JDK](http://www.oracle.com/technJava Runtime inetwork/java/javase/downloads/jdk8-downloads-2133151.html), Grider, etc.)

### Manual Installation
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

#### In Case of Failures
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
  libxml2-dev libxslt-dev zlib1g-dev net-tools
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
wget http://dl.fedoraproject.org/pub/epel/7/x86_64/e/epel-release-7-10.noarch.rpm
sudo rpm -ivh epel-release-7-10.noarch.rpm
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
sudo yum install java-1.8.0-openjdk-headless.x86_64 java-1.8.0-openjdk-devel.x86_64 \
  python-devel.x86_64 libxml2-devel.x86_64 libxslt-devel.x86_64 zlib.x86_64 gcc.x86_64
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
sudo docker run -it --rm -v /tmp/my-test:/bzt-configs blazemeter/taurus my-config.yml
```

Make note that `/tmp/my-test` was passed in `-v` Docker option, it's crucial. Here's [what happens](https://github.com/Blazemeter/taurus/blob/master/Dockerfile) inside the container:
 1. Directory `/tmp/my-test` is mounted as `/bzt-configs`
 1. Current directory changed to `/bzt-configs`
 1. Taurus is started with the config files you specified: `bzt /bzt-configs/my-config.yml

You can also specify multile config files in the `docker run` command with wildcards or as separate arguments like so:

```bash
sudo docker run -it --rm -v /tmp/my-test:/bzt-configs blazemeter/taurus *.yml

sudo docker run -it --rm -v /tmp/my-test:/bzt-configs blazemeter/taurus my-config-1.json my-config-2.json
```

### Additional Taurus Command-Line Options

You can still pass [command-line options](https://github.com/Blazemeter/taurus/blob/master/site/dat/docs/CommandLine.md) to Taurus through the Docker image. To do so, add the command line option at the end of the `docker run` command like so:

```bash
sudo docker run -it --rm -v /tmp/my-test:/bzt-configs blazemeter/taurus my-config-1.yml -o scenarios.sample.data-sources.0=data.csv
```


### Accessing Taurus Artifacts
If you want to receive Taurus artifacts from container, just mount some directory as `/tmp/artifacts` and files will get there. Following example gives you artifacts in `/tmp/my-run-artifacts` directory.

```bash
sudo docker run -it --rm -v /tmp:/bzt-configs -v /tmp/my-run-artifacts:/tmp/artifacts blazemeter/taurus
```

UNSTABLE_SNAPSHOT
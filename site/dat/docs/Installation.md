# Installing and Upgrading

The simplest Taurus installation requires you to use `pip` package manager:

```
pip install bzt
```  

and for upgrade:

```
pip install --upgrade bzt
```

Keep in mind that some additional software can be required depend of test type. 
(Java, specific python tools like locust, etc.)
Below you can see some specific info for different operation systems.

## Linux

You will need Python 3.7+ and Java installed. To install Taurus on Linux, do the following instructions:

```
sudo apt-get update
sudo apt-get install python3 default-jre-headless python3-tk python3-pip python3-dev \
  libxml2-dev libxslt-dev zlib1g-dev net-tools
sudo python3 -m pip install bzt
```
Upgrading to latest is as simple as this:

```
sudo python3 -m pip install --upgrade bzt
```

Alternatively, you can install using virtualenv, and you won't need sudo privileges for Taurus. 
Please see [virtualenv](https://virtualenv.pypa.io/en/stable/installation/) instructions for setting up 
your Python virtual environments, if you haven't done so already.

Then, to install Taurus, type:

```
pip install bzt
```

Upgrade is only:

```
pip install --upgrade bzt
```

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

NOTE: There is an issue with brew installation connected with numpy. 
In order to avoid this problem we suggest to install taurus using `pip` tool.

To install taurus with `pip` you need command line developers tools and python 3.6+ installed.
Then you need to install `Cython` if it is not installed using the following command:
```
pip3 install Cython
```

Then just install bzt:
```
pip3 install bzt
```

To upgrade use:

```
pip3 install --upgrade bzt
```

## Windows
Unfortunately windows installer is unsupported now. Please use standard python mechanism to get Taurus 
(see the top of the page).

1. Get Python 3.7+ from [http://www.python.org/downloads](http://www.python.org/downloads) and install it, 
don't forget to enable "Add python.exe to Path" checkbox.
1. Get latest Java from [https://www.java.com/download/](https://www.java.com/download/) and install it.

### Installing Additional Packages
If you want to install additional Python packages along with Taurus (for example Locust, Molotov, etc),
you should use `pip`, also `setuptools` and `wheel` packages and the latest 
[Microsoft Visual C++](https://visualstudio.microsoft.com/thank-you-downloading-visual-studio/?sku=Community&rel=16)
 might be necessary.
![Windows Tools Installation](win-tools-install.png)

Please check that the 'Desktop Development with C++' box is checked during installation. 

## Docker Image

Taurus has [Docker image](https://hub.docker.com/r/blazemeter/taurus/) that allows you to run tool as container.

To use it, create a directory, for example `/tmp/my-test`, put all configs and additional files like JMXses there, 
then start Docker like this:

```
docker run -it --rm -v /tmp/my-test:/bzt-configs blazemeter/taurus my-config.yml
```

Make note that `/tmp/my-test` was passed in `-v` Docker option, it's crucial. 
Here's [what happens](https://github.com/Blazemeter/taurus/blob/master/Dockerfile) inside the container:
 1. Directory `/tmp/my-test` is mounted as `/bzt-configs`
 1. Current directory changed to `/bzt-configs`
 1. Taurus is started with the config files you specified: `bzt /bzt-configs/my-config.yml

You can also specify multile config files in the `docker run` command with wildcards or as 
separate arguments like so:

```
docker run -it --rm -v /tmp/my-test:/bzt-configs blazemeter/taurus *.yml
docker run -it --rm -v /tmp/my-test:/bzt-configs blazemeter/taurus my-config-1.json my-config-2.json
```

### Additional Taurus Command-Line Options

You can still pass [command-line options](https://github.com/Blazemeter/taurus/blob/master/site/dat/docs/CommandLine.md) 
to Taurus through the Docker image. To do so, add the command line option at the end of the `docker run` command like so:

```bash
docker run -it --rm -v /tmp/my-test:/bzt-configs blazemeter/taurus my-config-1.yml -o scenarios.sample.data-sources.0=data.csv
```


### Accessing Taurus Artifacts
If you want to receive Taurus artifacts from container, just mount some directory as `/tmp/artifacts` and 
files will get there. Following example gives you artifacts in `/tmp/my-run-artifacts` directory.

```bash
docker run -it --rm -v /tmp:/bzt-configs -v /tmp/my-run-artifacts:/tmp/artifacts blazemeter/taurus
```

UNSTABLE_SNAPSHOT

## Data Collection Disclaimer

We have [Usage Statisctics](/bzt-usage-stats) on our website. That's why we collect the name of desktop OS, 
where you have run Taurus.


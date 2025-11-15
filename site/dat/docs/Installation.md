# Installing and Upgrading

Before Taurus and after Python installation, check if you have the following modules installed:
```
pip install --upgrade wheel setuptools
```

The simplest Taurus installation uses the `pip` package manager:

```
pip install bzt
```  

and for upgrade:

```
pip install --upgrade bzt
```

Keep in mind that some additional software can be required depending on the test type
(Java, specific Python tools like Locust, etc.).
Below, you see specific installation info for different operating systems.

## Unstable features
The following note indicates a features will be released soon:

_This is available only in [unstable snapshot](https://gettaurus.org/install/Installation/#Latest-Unstable-Snapshot)._

At the moment, this feature can't be installed from PyPi and is available from the linked page only.

## Linux

First verify that you have installed Python 3.9+ and Java. 

To install Taurus on Linux, follow these steps:

```
sudo apt-get update
sudo apt-get install python3 default-jre-headless python3-tk python3-pip python3-dev libxml2-dev libxslt-dev zlib1g-dev net-tools
sudo python3 -m pip install bzt
```
To upgrading to the latest release:

```
sudo python3 -m pip install --upgrade bzt
```

Alternatively, you can install it using virtualenv, and you won't need sudo privileges for Taurus. 
Please see [virtualenv](https://virtualenv.pypa.io/en/latest/installation.html) instructions for setting up 
your Python virtual environments, if you haven't done so already.

Then, to install Taurus, type:

```
pip install bzt
```

Upgrade is simply:

```
pip install --upgrade bzt
```

## Mac OS
### Install the Homebrew Package
Use the [brew](https://brew.sh/) package manager to install Taurus:
```bash
brew install bzt
```
and to update it:
```bash
brew upgrade bzt
```
If your brew auto update is switched off, manage it manually.

NOTE: There is an issue with brew installation connected with numpy. 
In order to avoid this problem, we suggest installing Taurus using the `pip` tool.

To install Taurus with `pip`, you need the command line developers tools and Python 3.9+ installed.
Then just install bzt:
```
pip3 install bzt
```

To upgrade, use:

```
pip3 install --upgrade bzt
```

## Windows

Preparation steps:

1. Get Python 3.9+ from [https://www.python.org/downloads](https://www.python.org/downloads) and install it. Enable the "Add python.exe to Path" checkbox.
2. Get the latest Java from [https://www.java.com/download/](https://www.java.com/download/) and install it.
3. Get the latest [Microsoft Visual C++](https://visualstudio.microsoft.com/thank-you-downloading-visual-studio/?sku=Community&channel=Release) and install it. Verify that the 'Desktop Development with C++' box is checked during installation. 

![Windows Tools Installation](win-tools-install.png)

Also, do not forget to update `pip`, `setuptools`, and `wheel` with the following command:
```
 python -m pip install --upgrade pip setuptools wheel
```

After all those steps, install Taurus:
```
python -m pip install bzt
```

## Docker Image

Taurus provides a [Docker image](https://hub.docker.com/r/blazemeter/taurus/) that lets you run the tool as container.

To use it, create a directory, for example `/tmp/my-test`, put all configs and additional files like JMXses there, 
then start Docker like this:

```
docker run -it --rm -v /tmp/my-test:/bzt-configs blazemeter/taurus my-config.yml
```

Note that `/tmp/my-test` was passed in the `-v` Docker option, it's crucial. 
Here's [what happens inside the container](https://github.com/Blazemeter/taurus/blob/master/Dockerfile):
 1. The directory `/tmp/my-test` is mounted as `/bzt-configs`.
 1. The current directory is changed to `/bzt-configs`.
 1. Taurus is started with the config files you specified: `bzt /bzt-configs/my-config.yml`.

You can also specify multiple config files in the `docker run` command with wildcards or as 
separate arguments, as shown in the following example:

```
docker run -it --rm -v /tmp/my-test:/bzt-configs blazemeter/taurus *.yml
docker run -it --rm -v /tmp/my-test:/bzt-configs blazemeter/taurus my-config-1.json my-config-2.json
```

### Additional Taurus Command-Line Options

You can still pass [command-line options](https://github.com/Blazemeter/taurus/blob/master/site/dat/docs/CommandLine.md) 
to Taurus through the Docker image. To do so, add the following command line option at the end of the `docker run` command:

```bash
docker run -it --rm -v /tmp/my-test:/bzt-configs blazemeter/taurus my-config-1.yml -o scenarios.sample.data-sources.0=data.csv
```


### Accessing Taurus Artifacts
If you want to receive Taurus artifacts from a container, mount a directory such as `/tmp/artifacts` and 
files will be stored there. The following example stores artifacts in the `/tmp/my-run-artifacts` directory:

```bash
docker run -it --rm -v /tmp:/bzt-configs -v /tmp/my-run-artifacts:/tmp/artifacts blazemeter/taurus
```

UNSTABLE_SNAPSHOT

## Data Collection Disclaimer

We may display anonymous [Usage Statistics](/bzt-usage-stats) on our website. 
That's why we collect the name of desktop OS, where you have run Taurus.

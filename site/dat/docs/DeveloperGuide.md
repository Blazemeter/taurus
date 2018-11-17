

# Rules for Contributing
 1. All contributions go via pull requests and code review
 1. Most of pylint warnings should be fixed ![](https://api.codacy.com/project/badge/Grade/9ed495a3e5df4ba2ad05e19a690121d2?ext=.svg)
 1. All new files must contain Apache License header (unit test files may not have it)
 1. All changes must be reflected in [Changelog](Changelog)
 1. All changes must be properly documented 
 1. All changes must be covered with unit tests, no broken tests in master ![](https://api.travis-ci.org/Blazemeter/taurus.svg?branch=master&ext=.svg)  ![](https://ci.appveyor.com/api/projects/status/github/Blazemeter/taurus?svg=true&ext=.svg) 
 
# Release Process
 - Modify [Changelog.md](Changelog), set version and date
 - Set correct version in `bzt/\_\_init\_\_.py`, commit
 - Create git tag, make `git push`, including tag
 - Build source distribution: ./build-sdist.sh
 - Upload to PyPi: `python ./setup.py upload`
 - rebuild and publish Docker image (how?)
 - site is updated automatically by Jenkins
 - notify all interested parties (Twitter, mailing lists)
 
# Deploying Project Website

If you want to deploy project website locally to debug its content use these instructions (actually it's for Debian/Ubuntu, adopt for your system if needed).   

Have Apache web server installed with  `mod\_rewrite` support and PHP 5:
```
  sudo apt-get update
  sudo apt-get install apache2 libapache2-mod-php5 php5
  sudo a2enmod rewrite
```
Prepare site directory, go here and execute
```
 php -r "readfile('https://getcomposer.org/installer');" | php
 php composer.phar update --prefer-stable
 ln -s vendor/undera/pwe/.htaccess 
 mkdir snapshots
``` 
Under `/etc/apache2/sites-enabled` create the file `taurus.conf` with following content (change for appropriate directory names):
```
<Directory "/home/mydir/taurus/site">
    Options FollowSymlinks
    AllowOverride All
    Require all granted
</Directory>
Listen 8002
<VirtualHost _default_:8002>
        DocumentRoot /home/mydir/taurus/site
</VirtualHost>
```
Restart Apache webserver 
```
 sudo service apache2 restart
```
Open [http://localhost:8002](http://localhost:8002) in your browser, you should see our website.

# Developing Taurus Extensions

Taurus is an extensible project. You can develop additional modules for it (executors, services, reporters)
in a completely separate codebase. See `bzt/resources/base-config.yml` to get a feeling of how Taurus
loads all its components.

Additionally, Taurus has a mechanism for automatically detecting configuration files for Taurus plugins.
Here's the conditions:
1. Your plugin has to be a Python package installed with the same Python that you're using to run Taurus.
1. You plugin has to have a `bzt-configs.json` file in the project dir (right next to `__init__.py`).
1. In this file should be a JSON list of configuration file names that your project uses to define Taurus modules.

For example, you are developing a Taurus extension with the following structure:

```
bzt_plugin_hello
├── __init__.py  # package init file, standard for Python
├── hello.py  # your plugin module, contains class HelloService
├── 10-hello.yml  # configuration file, analogous to base-config.yml
├── bzt-configs.json  # configuration index
```

Contents of `bzt-configs.json` should be `["10-hello.yml"]`.

Contents of `10-hello.yml`:
```yaml
modules:
  hello:
    class: bzt_plugin_hello.hello.HelloService
```

# Python Egg Snapshots

Download and install it like this:
```bash
pip install bzt-150.tar.gz
```

----

<downloads-dir:snapshots>



# Rules for Contributing
 1. All contributions goes through pull requests.
 1. Most of pylint warnings should be fixed.
 1. All new files must contain Apache License header (unit test files may not have it).
 1. All changes must be reflected in [Changelog](Changelog)
 1. All changes must be properly documented 
 1. All changes must be covered with unit tests, no broken tests in master.
  - [](https://api.travis-ci.org/Blazemeter/taurus.svg) 
  - [](https://ci.appveyor.com/api/projects/status/github/Blazemeter/taurus?svg=true&ext=.svg) 
  - [](https://scrutinizer-ci.com/g/Blazemeter/taurus/badges/quality-score.png?b=master&ext=.svg)
 
# Release Process
 - Modify Changelog.md and README.md
 - Set correct version in `bzt/\_\_init\_\_.py`, commit
 - Create git tag
 - git push, including tag
 - Upload to PyPi: `python ./setup.py clean sdist upload`
 - site is updated automatically by Jenkins
 
# Deploying Project Website

If you want to deploy project website locally to debug its content use this instruction (actually it's for Debuan/Ubuntu, adopt for your system if needed).   

Have Apache web server installed with  `mod\_rewrite` support and PHP 5:
```
  sudo aptitude update
  sudo aptitude install apache2 libapache2-mod-php5 php5
  sudo a2enmod rewrite
```
Prepare site directory, for it go to there and execute
```
 php -r "readfile('https://getcomposer.org/installer');" | php
 composer.phar update --prefer-stable
 cp vendor/undera/pwe/.htaccess .
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

# Dev Artifacts
[Code Coverage Report](/coverage/)

# Python Egg Snapshots

Download and install it like this:
```bash
pip install bzt-150.tar.gz
```

----

<downloads-dir:snapshots>


# Rules for Contributing
 1. All contributions goes through pull requests
  1. Most of pylint warnings should be fixed before accepting pull request
  2. All new files must contain Apache License header (unit test files may not have it)
 2. All changes must be covered with unit tests, no broken tests in master [](https://api.travis-ci.org/Blazemeter/taurus.svg)
 3. All changes must be reflected in [Changelog](Changelog)
 4. All changes must be properly documented 

# Release Process
 - Modify Changelog.md and README.md
 - Set correct version in `bzt/\_\_init\_\_.py`, commit
 - Create git tag
 - git push, including tag
 - Upload to PyPi: `python ./setup.py clean sdist upload`
 - site is updated automatically by Jenkins
 
# Deploying Project Website
 
If you want to deploy project website locally to debug its content, have Apache web server installed and configure it to point to `site` directory like this:
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

Then get [PHP composer](https://getcomposer.org/download/), go to `site` dir and run `composer.phar update`. Then open [http://localhost:8002](http://localhost:8002) in your browser, you should see our website.

# Dev Artifacts
[Code Coverage Report](/coverage/)

## Python Egg Snapshots

Download and install it like this:
```bash
pip install bzt-150.tar.gz
```

----

<downloads-dir:snapshots>
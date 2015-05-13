[![Build Status](https://api.travis-ci.org/Blazemeter/taurus.svg)](https://travis-ci.org/Blazemeter/taurus)

# Rules for Contributing
 1. All contributions goes through pull requests
 1.1 Most of pyling warnings should be fixed before accepting pull request
 1.2 All new files must contain Apache License header (unit test files may not have it)
 2. All changes must be covered with unit tests, no broken tests
 3. All changes must be reflected in [Changelog](Changelog)
 4. All changes must be properly documented 

# Release Process
 - Modify Changelog.md and README.md
 - Set correct version, commit
 - Create git tag
 - git push, including tag
 - Upload tp PyPi: `./make-docs.sh &&  python ./setup.py clean sdist upload upload_docs --upload-dir=build/docs`
 - generate and upload docs (TODO)
 

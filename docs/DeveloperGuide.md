# Rules for Contributing
 1. All contributions goes through pull requests
 1.1 Most of pyling warnings should be fixed before accepting pull request
 2. All changes must be covered with unit tests
 3. All changes must be reflected in [Changelog](Changelog)
 4. All changes must be properly documented 

# Release Process
 - Modify Changelog.md and README.md
 - Set correct version, commit
 - Create git tag
 - git push, including tag
 - Upload tp PyPi: `setup.py clean sdist upload`
 - generate and upload docs (TODO)
 

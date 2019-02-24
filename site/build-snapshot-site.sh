#!/bin/bash -xe

# copy snapshot to storage
gsutil cp -s regional dist/*.whl gs://taurus-site/snapshots/
gsutil cp -s regional build/nsis/*.exe gs://taurus-site/snapshots/

# cleanup site dir
mkdir site.bak
cp -v site/Dockerfile* site.bak
rm -r site

# clone base site
gsutil cp gs://taurus-site/site.zip site.zip
unzip -v site.zip -d .
cp -r site.bak/ site/

# static learning course
gsutil cp gs://taurus-site/learn.zip learn.zip
unzip learn.zip -d .
rm learn.zip

# add snapshots
gsutil cp gs://taurus-site/snapshots/*.whl snapshots
gsutil cp gs://taurus-site/snapshots/*.exe snapshots

cd ..
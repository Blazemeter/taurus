#!/bin/bash -xe

# copy snapshot to storage
gsutil cp dist/*.whl gs://taurus-site/snapshots/

# cleanup site dir
rm -r site
mkdir site
cd site

# clone base site
gsutil cp gs://taurus-site/site.zip site.zip
unzip site.zip -d .
rm site.zip

# add snapshots
mkdir snapshots
gsutil cp gs://taurus-site/snapshots/*.whl snapshots

cd ..
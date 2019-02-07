#!/bin/bash -xe

# copy snapshot to storage
gsutil cp dist/bzt-*.whl gs://taurus-site/snapshots/

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
gsutils cp gs://taurus-site/snapshots/ snapshots
gsutils cp gs://taurus-site/snapshots/blazemeter-pbench-extras_0.1.10.1_amd64.deb snapshots


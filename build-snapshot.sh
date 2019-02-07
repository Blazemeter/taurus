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
gsutil cp gs://taurus-site/blazemeter-pbench-extras_0.1.10.1_amd64.deb snapshots


# debug commands
zip -r site2.zip *
cd ..
# upload base site.zip to storage
gsutil cp site/site2.zip gs://taurus-site/
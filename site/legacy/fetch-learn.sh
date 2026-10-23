#!/bin/bash -e
# Downloads the "learn" course content (same source as site/deploy-site.sh), requires gsutil access to gs://taurus-site
cd "$(dirname "$0")"

rm -rf learn
gsutil cp gs://taurus-site/learn.zip learn.zip
unzip -q -o learn.zip
rm learn.zip

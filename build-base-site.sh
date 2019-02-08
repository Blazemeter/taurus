#!/bin/bash -xe

TAURUS_VERSION=$(python -c 'import bzt; print(bzt.VERSION)')
sed -ri "s/_TAURUS_VERSION_/_${TAURUS_VERSION}_/" site/dat/docs/Installation.md

# copy installer to storage
gsutil cp build/nsis/*.exe gs://taurus-site/releases/

mkdir -p site/msi
gsutil cp gs://taurus-site/releases/*.exe site/msi

python site/Taurus/kwindexer.py site/dat/docs site/dat/docs/KeywordIndex.md
cp site/dat/docs/img/*.png site/img/

# create base site zip
cd site
zip -r site.zip *
cd ..

# upload base site.zip to storage
gsutil cp site/site.zip gs://taurus-site/



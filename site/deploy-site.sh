#!/bin/bash -xe

GOOGLE_STORAGE="https:\/\/storage.cloud.google.com\/taurus-site\/"
UNSTABLE_SNAPSHOT=""
TAURUS_VERSION=$(python3 -c 'from bzt.resources.version import VERSION; print(VERSION)')

mkdir -p site/builds
PREFIX="\/builds\/"

if [ "$1" != "true" ]; then
    SNAPSHOT_VERSION="${TAURUS_VERSION}.${BUILD_NUMBER}"
    WHL_NAME="bzt-${SNAPSHOT_VERSION}-py2.py3-none-any.whl"
    gsutil cp -s regional dist/*.whl gs://taurus-site/snapshots/${WHL_NAME}

    # copy unstable snapshots into site
    cp dist/*.whl site/builds/${WHL_NAME}

    # prepare content for installation docs
    SNAPSHOT_HEADER="## Latest Unstable Snapshot"
    WHL_SNAPSHOT="Python wheel package: [${WHL_NAME}](${PREFIX}${WHL_NAME})"
    UNSTABLE_SNAPSHOT=${SNAPSHOT_HEADER}\\n\\n${WHL_SNAPSHOT}
fi

sed -ri "s/UNSTABLE_SNAPSHOT/${UNSTABLE_SNAPSHOT}/" site/dat/docs/Installation.md

python3 site/Taurus/kwindexer.py site/dat/docs site/dat/docs/KeywordIndex.md

gsutil cp gs://taurus-site/learn.zip learn.zip
unzip -o learn.zip -d site
rm learn.zip

docker build -t taurus-site.${BUILD_NUMBER} site

gcloud auth --quiet configure-docker

docker tag taurus-site.${BUILD_NUMBER} us.gcr.io/${PROJECT_ID}/taurus-site:latest
docker push us.gcr.io/${PROJECT_ID}/taurus-site:latest
gcloud container clusters get-credentials taurus-site

kubectl set image deployment/taurus-site taurus-site=us.gcr.io/${PROJECT_ID}/taurus-site:latest
kubectl delete pod -l run=taurus-site

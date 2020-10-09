#!/bin/bash -xe

GOOGLE_STORAGE="https:\/\/storage.cloud.google.com\/taurus-site\/"
UNSTABLE_SNAPSHOT=""
TAURUS_VERSION=$(python3 -c 'from bzt.resources.version import VERSION; print(VERSION)')
STABLE_EXE_NAME=TaurusInstaller_${TAURUS_VERSION}_x64.exe

mkdir site/builds
PREFIX="\/builds\/"

if [ "$1" = "true" ]; then
    gsutil cp build/nsis/*.exe gs://taurus-site/releases/
else
    SNAPSHOT_VERSION="${TAURUS_VERSION}.${BUILD_NUMBER}"
    WHL_NAME="bzt-${SNAPSHOT_VERSION}-py2.py3-none-any.whl"
    EXE_NAME="TaurusInstaller_${SNAPSHOT_VERSION}_x64.exe"
    gsutil cp -s regional dist/*.whl gs://taurus-site/snapshots/${WHL_NAME}
    gsutil cp -s regional build/nsis/*.exe gs://taurus-site/snapshots/${EXE_NAME}

    # copy unstable snapshots into site
    cp dist/*.whl site/builds/${WHL_NAME}
    cp build/nsis/*.exe site/builds/${EXE_NAME}

    # prepare content for installation docs
    SNAPSHOT_HEADER="## Latest Unstable Snapshots"
    WHL_SNAPSHOT="Python wheel package: [${WHL_NAME}](${PREFIX}${WHL_NAME})"
    EXE_SNAPSHOT="Windows installer: [${EXE_NAME}](${PREFIX}${EXE_NAME})"
    UNSTABLE_SNAPSHOT=${SNAPSHOT_HEADER}\\n\\n${WHL_SNAPSHOT}\\n\\n${EXE_SNAPSHOT}
fi

gsutil cp gs://taurus-site/releases/${STABLE_EXE_NAME} site/builds
RELEASE_SNAPSHOT="${PREFIX}${STABLE_EXE_NAME}"

sed -ri "s/RELEASE_SNAPSHOT/${RELEASE_SNAPSHOT}/" site/dat/docs/Installation.md
sed -ri "s/UNSTABLE_SNAPSHOT/${UNSTABLE_SNAPSHOT}/" site/dat/docs/Installation.md

python3 site/Taurus/kwindexer.py site/dat/docs site/dat/docs/KeywordIndex.md

gsutil cp gs://taurus-site/learn.zip learn.zip
unzip -o learn.zip -d site
rm learn.zip

docker build -t taurus-site.${BUILD_NUMBER} site

gcloud auth --quiet configure-docker

docker tag taurus-site.${BUILD_NUMBER} gcr.io/${PROJECT_ID}/taurus-site.${BUILD_NUMBER}
docker push gcr.io/${PROJECT_ID}/taurus-site.${BUILD_NUMBER}
gcloud container clusters get-credentials taurus-site

kubectl set image deployment/taurus-site taurus-site=gcr.io/${PROJECT_ID}/taurus-site.${BUILD_NUMBER}

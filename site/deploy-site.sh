#!/bin/bash -xe

gcloud auth activate-service-account --key-file ${KEY_FILE}
gcloud config set project ${PROJECT_ID}
gcloud config set compute/zone us-central1-a

GOOGLE_STORAGE="https:\/\/storage.cloud.google.com\/taurus-site\/"
UNSTABLE_SNAPSHOT=""
TAURUS_VERSION=$(python -c 'import bzt; print(bzt.VERSION)')

if [ "$1" = "true" ]; then
    gsutil cp build/nsis/*.exe gs://taurus-site/releases/
else
    SNAPSHOT_HEADER="## Latest Unstable Snapshots"
    WHL_SNAPSHOT="Python wheel package: [bzt-${TAURUS_VERSION}.${BUILD_NUMBER}-py2.py3-none-any.whl](${GOOGLE_STORAGE}snapshots\/bzt-${TAURUS_VERSION}.${BUILD_NUMBER}-py2.py3-none-any.whl)"
    EXE_SNAPSHOT="Windows installer: [TaurusInstaller_${TAURUS_VERSION}.${BUILD_NUMBER}_x64.exe](${GOOGLE_STORAGE}snapshots\/TaurusInstaller_${TAURUS_VERSION}.${BUILD_NUMBER}_x64.exe)"
    UNSTABLE_SNAPSHOT=${SNAPSHOT_HEADER}/\n/\n${WHL_SNAPSHOT}/\n/\n${EXE_SNAPSHOT}

    gsutil cp -s regional dist/*.whl gs://taurus-site/snapshots/
    gsutil cp -s regional build/nsis/*.exe gs://taurus-site/snapshots/
fi

sed -ri "s/RELEASE_SNAPSHOT/${GOOGLE_STORAGE}releases\/TaurusInstaller_${TAURUS_VERSION}_x64.exe/" site/dat/docs/Installation.md
sed -ri "s/UNSTABLE_SNAPSHOT/${UNSTABLE_SNAPSHOT}/" site/dat/docs/Installation.md

python site/Taurus/kwindexer.py site/dat/docs site/dat/docs/KeywordIndex.md
#cp site/dat/docs/img/*.png site/img/

gsutil cp gs://taurus-site/learn.zip learn.zip
unzip -o learn.zip -d site
rm learn.zip

docker build -t taurus-site.${BUILD_NUMBER} site

gcloud auth --quiet configure-docker

docker tag  taurus-site.${BUILD_NUMBER} gcr.io/${PROJECT_ID}/taurus-site.${BUILD_NUMBER}
docker push gcr.io/${PROJECT_ID}/taurus-site.${BUILD_NUMBER}
gcloud container clusters get-credentials taurus-site

kubectl set image deployment/taurus-site taurus-site=gcr.io/${PROJECT_ID}/taurus-site.${BUILD_NUMBER}

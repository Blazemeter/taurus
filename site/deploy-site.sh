#!/bin/bash -xe

gcloud auth activate-service-account --key-file ${KEY_FILE}
gcloud config set project ${PROJECT_ID}
gcloud config set compute/zone us-central1-a

GOOGLE_STORAGE="https:\/\/storage.cloud.google.com\/taurus-site\/"
UNSTABLE_SNAPSHOT=""
TAURUS_VERSION=$(python -c 'import bzt; print(bzt.VERSION)')

mkdir site/builds
PREFIX="\/builds\/"

if [ "$1" = "true" ]; then
    gsutil cp build/nsis/*.exe gs://taurus-site/releases/
else
    gsutil cp -s regional dist/*.whl gs://taurus-site/snapshots/
    gsutil cp -s regional build/nsis/*.exe gs://taurus-site/snapshots/

    # copy unstable snapshots into site
    cp dist/*.whl site/builds
    cp build/nsis/*.exe site/builds

    # prepare content for installation docs
    SNAPSHOT_HEADER="## Latest Unstable Snapshots"
    WHL_SNAPSHOT="Python wheel package: [bzt-${TAURUS_VERSION}-py2.py3-none-any.whl](${PREFIX}bzt-${TAURUS_VERSION}-py2.py3-none-any.whl)"
    EXE_SNAPSHOT="Windows installer: [TaurusInstaller_${TAURUS_VERSION}_x64.exe](${PREFIX}TaurusInstaller_${TAURUS_VERSION}_x64.exe)"
    UNSTABLE_SNAPSHOT=${SNAPSHOT_HEADER}\\n\\n${WHL_SNAPSHOT}\\n\\n${EXE_SNAPSHOT}

    # cut BUILD_NUMBER off
    TAURUS_VERSION=${TAURUS_VERSION%.*}
fi

STABLE_EXE=TaurusInstaller_${TAURUS_VERSION}_x64.exe
gsutil cp gs://taurus-site/releases/${STABLE_EXE} site/builds
RELEASE_SNAPSHOT="${PREFIX}${STABLE_EXE}"

sed -ri "s/RELEASE_SNAPSHOT/${RELEASE_SNAPSHOT}/" site/dat/docs/Installation.md
sed -ri "s/UNSTABLE_SNAPSHOT/${UNSTABLE_SNAPSHOT}/" site/dat/docs/Installation.md

python site/Taurus/kwindexer.py site/dat/docs site/dat/docs/KeywordIndex.md

gsutil cp gs://taurus-site/learn.zip learn.zip
unzip -o learn.zip -d site
rm learn.zip

TODAY_DATE=$(date +'%d.%m.%Y')
gsutil cp gs://taurus-statistic/stats_${TODAY_DATE}.csv site/bzt-usage-stats/stats_${TODAY_DATE}.csv
gsutil cp gs://taurus-statistic/home_users.json site/bzt-usage-stats/home_users.json
gsutil cp gs://taurus-statistic/launch_by_day.json site/bzt-usage-stats/launch_by_day.json
gsutil cp gs://taurus-statistic/launch_by_week.json site/bzt-usage-stats/launch_by_week.json
gsutil cp gs://taurus-statistic/launch_by_month.json site/bzt-usage-stats/launch_by_month.json
gsutil cp gs://taurus-statistic/launch_by_platform.json site/bzt-usage-stats/launch_by_platform.json
gsutil cp gs://taurus-statistic/new_users_by_day.json site/bzt-usage-stats/new_users_by_day.json

docker build -t taurus-site.${BUILD_NUMBER} site

gcloud auth --quiet configure-docker

docker tag  taurus-site.${BUILD_NUMBER} gcr.io/${PROJECT_ID}/taurus-site.${BUILD_NUMBER}
docker push gcr.io/${PROJECT_ID}/taurus-site.${BUILD_NUMBER}
gcloud container clusters get-credentials taurus-site

kubectl set image deployment/taurus-site taurus-site=gcr.io/${PROJECT_ID}/taurus-site.${BUILD_NUMBER}

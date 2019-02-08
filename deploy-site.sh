#!/bin/bash -xe

gcloud auth activate-service-account --key-file ${KEY_FILE}
gcloud config set project ${PROJECT_ID}
gcloud config set compute/zone us-central1-a

#if [ "$1" = "true" ]; then
    ./build-base-site.sh
#fi

./build-snapshot-site.sh

cd site
docker build -t taurus-site.${BUILD_NUMBER} .

gcloud auth --quiet configure-docker

docker tag  taurus-site.${BUILD_NUMBER} gcr.io/${PROJECT_ID}/taurus-site.${BUILD_NUMBER}
docker push gcr.io/${PROJECT_ID}/taurus-site.${BUILD_NUMBER}
gcloud container clusters get-credentials taurus-site

kubectl set image deployment/taurus-site taurus-site=gcr.io/${PROJECT_ID}/taurus-site.${BUILD_NUMBER}

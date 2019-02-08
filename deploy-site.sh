#!/bin/bash -xe

gcloud auth activate-service-account --key-file ${KEY_FILE}
gcloud config set project ${PROJECT_ID}
gcloud config set compute/zone us-central1-a

./build-base-site.sh
./build-snapshot-site.sh

docker build -t taurus-site.${BUILD_NUMBER} .

gcloud auth configure-docker

docker tag  ${IMAGE_TAG} gcr.io/${PROJECT_ID}/taurus-site.${BUILD_NUMBER}
docker push gcr.io/${PROJECT_ID}/taurus-site.${BUILD_NUMBER}
gcloud container clusters get-credentials taurus-site
#kubectl set image deployment/taurus-site taurus-site=gcr.io/${PROJECT_ID}/taurus-site.${BUILD_NUMBER}
kubectl run taurus-site --image=gcr.io/${PROJECT_ID}/taurus-site.${BUILD_NUMBER} --port 80
kubectl expose deployment taurus-site --type=LoadBalancer --port 80 --target-port 80
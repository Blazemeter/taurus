#!/bin/bash -xe

gcloud auth activate-service-account --key-file ${KEY_FILE}
gcloud config set project ${PROJECT_ID}
gcloud config set compute/zone us-central1-a

./build-base-site.sh
./build-snapshot-site.sh
./deploy-site.sh

docker build -t ${IMAGE_TAG} .

gcloud auth configure-docker

docker tag  ${IMAGE_TAG} gcr.io/${PROJECT_ID}/${IMAGE_TAG}
docker push gcr.io/${PROJECT_ID}/${IMAGE_TAG}
gcloud container clusters get-credentials taurus-site
#kubectl set image deployment/taurus-site taurus-site=gcr.io/${PROJECT_ID}/${IMAGE_TAG}
kubectl run taurus-site --image=gcr.io/${PROJECT_ID}/${IMAGE_TAG} --port 80
kubectl expose deployment taurus-site --type=LoadBalancer --port 80 --target-port 80
#!/bin/bash -xe

docker build -t ${IMAGE_TAG} .

gcloud auth configure-docker

docker tag  ${IMAGE_TAG} gcr.io/${PROJECT_ID}/${IMAGE_TAG}
docker push gcr.io/${PROJECT_ID}/${IMAGE_TAG}
gcloud container clusters get-credentials taurus-site
kubectl set image deployment/taurus-site taurus-site=gcr.io/${PROJECT_ID}/${IMAGE_TAG}
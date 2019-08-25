#!/bin/bash -xe

gcloud auth activate-service-account --key-file ${GOOGLE_CLOUD_CRED}
gcloud config set project blazemeter-taurus-website-prod
gcloud config set compute/zone us-central1-a

python /var/www/html/bzt-usage-stats/aggregate_stat.py
gsutil cp /var/www/html/bzt-usage-stats/*.json gs://taurus-statistic/
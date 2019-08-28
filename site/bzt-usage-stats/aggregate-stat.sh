#!/bin/bash -xe

YESTERDAY=$(date -d "1 day ago" '+%d.%m.%Y')
python /var/www/html/bzt-usage-stats/aggregate_stat.py
rm /var/www/html/bzt-usage-stats/stats_${YESTERDAY}.csv

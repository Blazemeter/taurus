# Frozen copy of the site build as it was deployed with release 1.17.1 (PHP 7.4 on Debian 11 bullseye).
# Used as a local baseline to compare against while site dependencies are being updated.
# Site sources come from the "site" build context = site/ of the 1.17.1 git tag (not the working tree),
# files missing in the tag (composer.lock, learn/) from the main context = this directory,
# see docker-compose.yml.

# Docs preprocessing done by site/deploy-site.sh for release builds
FROM python:3.12-slim AS docs
RUN pip install --no-cache-dir pyyaml==6.0.2
WORKDIR /site
COPY --from=site Taurus/kwindexer.py ./Taurus/
COPY --from=site dat ./dat
RUN sed -ri "s/UNSTABLE_SNAPSHOT//" dat/docs/Installation.md && \
    python3 Taurus/kwindexer.py dat/docs dat/docs/KeywordIndex.md

FROM php:7-apache@sha256:c9d7e608f73832673479770d66aacc8100011ec751d1905ff63fae3fe2e0ca6d

# bullseye packages were moved from deb.debian.org to archive.debian.org after its LTS ended
RUN a2enmod rewrite && \
    sed -i "s#deb.debian.org#archive.debian.org#g" /etc/apt/sources.list && \
    apt-get update && \
    apt-get install -y --no-install-recommends git zip unzip libyaml-dev libpq-dev && \
    docker-php-ext-install pdo pdo_pgsql pgsql && \
    curl --silent --show-error https://getcomposer.org/installer | php -- --version=2.10.3 && \
    mv composer.phar /usr/bin && \
    pecl install yaml-2.0.4 && \
    echo "extension=yaml.so" > /usr/local/etc/php/conf.d/ext-yaml.ini

COPY --from=site php.ini /usr/local/etc/php/

# composer.lock captured from the working build, keeps dev-master dependencies (undera/pwe etc.) frozen
COPY --from=site composer.json ./
COPY composer.lock ./
RUN composer.phar install --prefer-dist --no-dev

RUN cp vendor/undera/pwe/.htaccess ./
COPY --from=site Taurus ./Taurus
COPY --from=docs /site/dat ./dat
COPY --from=site img ./img
RUN mkdir builds
COPY learn ./learn

COPY --from=site bzt-usage-stats ./bzt-usage-stats
RUN chmod 777 -R bzt-usage-stats/
COPY --from=site *.php ./

# Local playground build of the site, mirrors site/Dockerfile with these differences:
# - blazemeter/pwe is installed from the local clone (../../../pwe) passed in as an additional
#   build context, see docker-compose.yml next to this file
# - PHP version can be switched, dependencies are resolved at build time (no lock file)
# - docs preprocessing done by site/deploy-site.sh for production builds runs in the docs stage

# PHP version of the site image, override with: docker compose build --build-arg PHP_VERSION=8.4
ARG PHP_VERSION=8.5

FROM python:3.12-slim AS docs
RUN pip install --no-cache-dir pyyaml==6.0.2
WORKDIR /site
COPY Taurus/kwindexer.py ./Taurus/
COPY dat ./dat
RUN sed -ri "s/UNSTABLE_SNAPSHOT//" dat/docs/Installation.md && \
    python3 Taurus/kwindexer.py dat/docs dat/docs/KeywordIndex.md

FROM php:${PHP_VERSION}-apache

COPY --from=composer/composer:2.10.3-bin /composer /usr/bin/composer

# apt-get upgrade applies Debian security updates published after the base image was built
RUN a2enmod rewrite && \
    apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y --no-install-recommends git unzip libyaml-dev libpq-dev && \
    docker-php-ext-install pgsql && \
    pecl install yaml-2.3.0 && \
    docker-php-ext-enable yaml && \
    rm -rf /var/lib/apt/lists/*

ADD php.ini /usr/local/etc/php/

# local clone, the same path is bind-mounted at runtime so edits are picked up without rebuild
COPY --from=pwe . /src/pwe

ADD dev/composer.json ./
RUN composer update --prefer-stable --no-dev --no-interaction
# same check as the production build: fail on dependencies with security advisories or abandoned ones
RUN composer audit --locked --no-dev

RUN cp vendor/blazemeter/pwe/.htaccess ./
ADD Taurus ./Taurus
COPY --from=docs /site/dat ./dat
ADD img ./img
RUN mkdir builds
ADD legacy/learn ./learn

COPY bzt-usage-stats ./bzt-usage-stats
RUN chmod 777 -R bzt-usage-stats/
ADD *.php ./

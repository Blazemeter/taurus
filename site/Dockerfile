FROM php:8.5-apache

# Composer binary from the image published by the Composer project, pinned to the tested version
COPY --from=composer/composer:2.10.3-bin /composer /usr/bin/composer

RUN a2enmod rewrite && \
    apt-get update && \
    apt-get install -y --no-install-recommends git unzip libyaml-dev libpq-dev && \
    docker-php-ext-install pgsql && \
    pecl install yaml-2.3.0 && \
    docker-php-ext-enable yaml && \
    rm -rf /var/lib/apt/lists/*

ADD php.ini /usr/local/etc/php/

# exact dependency versions from the committed lock file
ADD composer.json composer.lock ./
RUN composer install --no-dev --no-interaction

RUN cp vendor/blazemeter/pwe/.htaccess ./
ADD Taurus ./Taurus
ADD dat ./dat
ADD img ./img
ADD builds ./builds
ADD learn ./learn

COPY bzt-usage-stats ./bzt-usage-stats
RUN chmod 777 -R bzt-usage-stats/
ADD *.php ./

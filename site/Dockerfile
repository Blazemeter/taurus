FROM php:7-apache

RUN a2enmod rewrite && \
    apt-get update && \
    apt-get install -y --no-install-recommends git zip unzip libyaml-dev && \
    curl --silent --show-error https://getcomposer.org/installer | php && \
    mv composer.phar /usr/bin && \
    pecl install yaml-2.0.4 && \
    echo "extension=yaml.so" > /usr/local/etc/php/conf.d/ext-yaml.ini

ADD php.ini /usr/local/etc/php/

ADD composer.json ./
RUN composer.phar update --prefer-stable --no-dev

RUN cp vendor/undera/pwe/.htaccess ./
ADD Taurus ./Taurus
ADD dat ./dat
ADD img ./img
ADD msi ./msi
ADD snapshots ./snapshots

#RUN apt install -y mc bash-completion
ADD *.php ./

RUN mkdir -p snapshots && mkdir -p msi
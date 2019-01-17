FROM php:7-apache

RUN a2enmod rewrite && \
    apt-get update && \
    apt-get install -y --no-install-recommends git zip unzip && \
    curl --silent --show-error https://getcomposer.org/installer | php && \
    mv composer.phar /usr/bin && \
    RUN pecl install yaml-2.0.4 && \
    echo "extension=yaml.so" > /usr/local/etc/php/conf.d/ext-yaml.ini

ADD php.ini /usr/local/etc/php/

ADD composer.json ./
RUN composer.phar update --prefer-stable --no-dev

ADD .htaccess ./
ADD Taurus ./Taurus
ADD dat ./dat
ADD img ./img

#RUN apt install -y mc bash-completion libyaml-dev
ADD *.php ./

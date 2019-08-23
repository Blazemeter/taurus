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
ADD builds ./builds
ADD learn ./learn

RUN apt-get update -y && \
	apt-get install -y \
        gnupg \
		cron \
		supervisor

RUN echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] http://packages.cloud.google.com/apt cloud-sdk main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list && curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key --keyring /usr/share/keyrings/cloud.google.gpg  add - && apt-get update -y && apt-get install google-cloud-sdk -y

# test crons added via crontab
RUN echo "@midnight sh /var/www/html/bzt-usage-stats/aggregate-stat.sh" | crontab -

# supervisord config file
COPY ./supervisord.conf /etc/supervisor/conf.d/supervisord.conf
COPY bzt-usage-stats ./bzt-usage-stats
RUN chmod 777 -R bzt-usage-stats/
#RUN apt install -y mc bash-completion
ADD *.php ./
CMD ["/usr/bin/supervisord"]

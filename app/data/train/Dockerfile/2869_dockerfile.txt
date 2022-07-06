FROM php:5.5-apache

MAINTAINER Sebastien Lepers seb@meliweb.fr

RUN apt-get update

# Magento needed PHP exts
RUN requirements="libpng12-dev libmcrypt-dev libmcrypt4 libcurl3-dev libfreetype6 libjpeg62-turbo libpng12-dev libfreetype6-dev libjpeg62-turbo-dev libxml2-dev" \
    && apt-get install -y $requirements \
    && docker-php-ext-install pdo_mysql \
    && docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-jpeg-dir=/usr/include/ \
    && docker-php-ext-install gd \
    && docker-php-ext-install mcrypt \
    && docker-php-ext-install mbstring \
    && docker-php-ext-install soap \
    && requirementsToRemove="libpng12-dev libmcrypt-dev libcurl3-dev libpng12-dev libfreetype6-dev libjpeg62-turbo-dev libxml2-dev" \
    && apt-get purge --auto-remove -y $requirementsToRemove

RUN usermod -u 1000 www-data
RUN a2enmod rewrite && \
    a2enmod headers
RUN sed -i -e 's/\/var\/www\/html/\/var\/www\/htdocs/' /etc/apache2/apache2.conf
COPY php.ini.sample /usr/local/etc/php/php.ini

WORKDIR /var/www/htdocs

# HTTPS
RUN apt-get install -y openssl

RUN mkdir -p /etc/apache2/external/ && openssl req -x509 -newkey rsa:4086 \
      -subj "/C=FR/ST=Paris/L=Paris/O=XXXX/CN=localhost" \
      -keyout "/etc/apache2/external/key.pem" \
      -out "/etc/apache2/external/cert.pem" \
      -days 3650 -nodes -sha256

RUN sed -i "s/SSLMutex/#SSLMutex/" /etc/apache2/mods-available/ssl.conf

RUN a2enmod ssl

ADD default-ssl.conf /etc/apache2/sites-enabled/001-default-ssl.conf

EXPOSE 443

# SMTP Mailcatcher
RUN apt-get install -y ssmtp
RUN sed -i "s/mailhub=mail/mailhub=my_smtp/" /etc/ssmtp/ssmtp.conf

# Clean
RUN apt-get clean && rm -rf /var/lib/apt/lists/*

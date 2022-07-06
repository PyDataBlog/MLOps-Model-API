FROM debian:wheezy

MAINTAINER Stephane Boulard <lossendae@gmail.com>

# Install Apache & enable Mod PHP
RUN apt-get update && apt-get install -y \
    apache2 \
    apache2-mpm-prefork \
    apache2-prefork-dev

RUN a2enmod rewrite vhost_alias rewrite headers include ssl

ADD config/svd.local.conf /etc/apache2/conf.d/
ADD config/ports.conf /etc/apache2/

ENV APACHE_RUN_USER www-data
ENV APACHE_RUN_GROUP www-data
ENV APACHE_LOG_DIR /var/log/apache2
ENV APACHE_PID_FILE /var/run/apache2.pid
ENV APACHE_RUN_DIR /var/run/apache2
ENV APACHE_LOCK_DIR /var/lock/apache2
ENV APACHE_SERVERADMIN admin@localhost
ENV APACHE_SERVERNAME localhost
ENV APACHE_SERVERALIAS docker.localhost
ENV APACHE_DOCUMENTROOT /var/www

ENV BUILD_DEPS autoconf \
                automake \
                libtool \
                re2c \
                flex \
                bison \
                build-essential \
                wget \
                libgd2-xpm-dev \
                mcrypt \
                libmcrypt-dev \
                libbz2-dev \
                libcurl4-openssl-dev \
                libevent-dev \
                libffi-dev \
                libglib2.0-dev \
                libjpeg-dev \
                libpng-dev \
                libtiff-dev \
                libgif-dev \
                libmagickcore-dev \
                libmagickwand-dev \
                libmysqlclient-dev \
                libncurses-dev \
                libpq-dev \
                libreadline-dev \
                libsqlite3-dev \
                libssl-dev \
                libxml2-dev \
                libxslt-dev \
                libyaml-dev \
                zlib1g-dev \
                libsslcommon2-dev

RUN set -x \
    && apt-get update && apt-get install -y $BUILD_DEPS --no-install-recommends

# Add libraries
ADD ./lib /usr/src

RUN rm -rf /var/lib/apt/lists/* \
    && mkdir -p /opt/php52 /usr/src/php

WORKDIR /usr/src/php

# Build and install PHP
RUN set -x \
    && tar -xf /usr/src/php52/php-5.2.17.tar.bz2 -C /usr/src/php --strip-components=1 \
    && gzip -cd /usr/src/patches/suhosin-patch-5.2.16-0.9.7.patch.gz > /tmp/suhosin.patch \
    && patch -p1 < /tmp/suhosin.patch \
    && patch -p1 < /usr/src/patches/php-5.2.17-libxml2.patch \
    && patch -p1 < /usr/src/patches/php-5.2.17-openssl.patch \
    && ./configure \
       --prefix=/opt/php52 \
       --with-libdir=/lib/x86_64-linux-gnu \
       --with-bcmath \
       --enable-bcmath \
       --with-bz2 \
       --enable-calendar \
       --with-calendar \
       --with-curl \
       --enable-exif \
       --enable-ftp \
       --with-ftp \
       --enable-gettext \
       --with-gettex \
       --with-gd \
       --enable-gd-native-ttf \
       --with-readline \
       --enable-mbstring \
       --enable-mime_magic \
       --with-mime_magic \
       --with-mysql \
       --with-mysqli \
       --enable-openssl \
       --with-openssl \
       --enable-shmop \
       --with-shmop \
       --enable-soap \
       --enable-sockets \
       --with-sockets \
       --enable-sysvsem \
       --with-sysvsem \
       --enable-sysvshm \
       --with-sysvshm \
       --enable-pcntl \
       --with-pdo-mysql \
       --with-pear=/opt/php52/pear \
       --enable-wddx \
       --with-wddx \
       --with-xsl \
       --enable-zip \
       --with-freetype-dir \
       --with-jpeg-dir=/usr \
       --with-png-dir=/usr \
       --with-zlib-dir \
       --with-mcrypt \
       --with-config-file-path=/opt/php52/conf \
       --with-config-file-scan-dir=/opt/php52/conf.d \
       --with-iconv-dir=/opt/local \
       --with-apxs2=/usr/bin/apxs2 \
    && sed -i 's/-lxml2 -lxml2 -lxml2/-lcrypto -lssl/' Makefile \
    && make -j"$(nproc)" \
    && make install \
    && { find /usr/local/bin /usr/local/sbin -type f -executable -exec strip --strip-all '{}' + || true; } \
    && make clean

# Add ini
ADD config/php.ini /opt/php52/conf/
ADD config/php5.conf /etc/apache2/mods-available/

RUN ln -s ../mods-available/php5.conf /etc/apache2/mods-enabled/php5.conf

# Register path to php bin allowing to be used from cmd
ENV PATH $PATH:/opt/php52/bin/

# Clean up
WORKDIR /

EXPOSE 80 443

ADD config/start.sh /start.sh
RUN chmod +x /start.sh \
    && mkdir -p /var/run/apache2/

CMD ["bash", "start.sh"]

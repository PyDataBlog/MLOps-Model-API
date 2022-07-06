FROM    debian:jessie

MAINTAINER Jan <jan@pronovix.com>

# Switch to de.debian.org
ADD ./sources.list			/etc/apt/sources.list

# Upgrade debian packages
RUN dpkg-divert --local --rename --add /sbin/initctl
RUN ln -sf /bin/true /sbin/initctl  
RUN DEBIAN_FRONTEND=noninteractive apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get -y upgrade
RUN DEBIAN_FRONTEND=noninteractive apt-get -y install php5-fpm php5-mysql php5-curl php5-gd php5-imagick imagemagick
#	php5-cgi php5-imap php5-intl php5-mcrypt php5-mongo php5-pgsql php5-readline php5-sqlite php5-tidy php5-xmlrpc php5-xsl php-pear
#	php5-dev php5-xdebug
RUN DEBIAN_FRONTEND=noninteractive apt-get autoclean

RUN sed -i '/daemonize /c daemonize = no' /etc/php5/fpm/php-fpm.conf && \
    sed -i '/^listen /c listen = 0.0.0.0:9000' /etc/php5/fpm/pool.d/www.conf && \
    sed -i 's/^listen.allowed_clients/;listen.allowed_clients/' /etc/php5/fpm/pool.d/www.conf

RUN sed -i 's/;cgi.fix_pathinfo=1/cgi.fix_pathinfo = 0/' /etc/php5/fpm/php.ini && \
    sed -i 's/zlib.output_compression = Off/zlib.output_compression = On/' /etc/php5/fpm/php.ini && \
    sed -i 's/;zlib.output_compression_level = -1/zlib.output_compression_level = 6/' /etc/php5/fpm/php.ini && \
    sed -i 's/expose_php = On/expose_php = Off/' /etc/php5/fpm/php.ini && \
    sed -i 's/display_errors = On/display_errors = Off/' /etc/php5/fpm/php.ini && \
    sed -i 's/allow_url_include = On/allow_url_include = Off/' /etc/php5/fpm/php.ini && \
    sed -i 's/;date.timezone =/date.timezone = Europe\/London/' /etc/php5/fpm/php.ini

VOLUME /var/www

EXPOSE 9000

CMD ["php5-fpm", "-F"]

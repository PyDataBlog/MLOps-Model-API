FROM maxexcloo/nginx-php
# Download HHVM
RUN wget -O - http://dl.hhvm.com/conf/hhvm.gpg.key | apt-key add -
RUN echo deb http://dl.hhvm.com/debian wheezy main | tee /etc/apt/sources.list.d/hhvm.list
RUN echo deb http://packages.dotdeb.org wheezy all | tee /etc/apt/sources.list.d/lamp.list
# Install packages
RUN apt-get update
RUN apt-get install -y php5-memcached hhvm
# Create required directories
RUN mkdir -p /var/log/supervisor
RUN mkdir -p /var/run/hhvm
# Add configuration files
ADD supervisord.conf /etc/supervisor/conf.d/hhvm.conf
ADD php.ini /etc/php5/fpm/conf.d/40-custom.ini
# Expose ports
EXPOSE 80 9000
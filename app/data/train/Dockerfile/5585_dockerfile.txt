# Drupal 7 Web Container
#
# VERSION               0.0.1
#

FROM debian:jessie
MAINTAINER Ericmaster <ericmaster@nimblersoft.com>

# Define mountable directories.
#VOLUME ["/etc/nginx/sites-enabled", "/etc/nginx/certs", "/etc/nginx/conf.d", "/var/log/nginx", "/var/www/html"]
VOLUME ["/etc/nginx/sites-enabled", "/var/log", "/var/www/html", "/var/www/_private", "/tmp"]

# Install nginx, php5-fpm, additional php extensions and avconv
RUN apt-get update \
  && apt-get install -y nginx php5-fpm php5-mysql php5-gd php5-curl php5-apcu php5-memcached curl lxc iptables

#RUN echo "y\ny" | pecl install apcu-beta
#ADD php/apcu.ini /usr/local/etc/php/conf.d/apcu.ini

# Run nginx in daemon mode
RUN echo "daemon off;" >> /etc/nginx/nginx.conf

#Install profiler
RUN apt-get install php-pear \
  && pear upgrade pear \
  && pecl install -f xhprof \
  && echo "extension=xhprof.so" >> /etc/php5/fpm/php.ini

# Customize php settings
RUN cp /etc/php5/fpm/php.ini /etc/php5/fpm/php.ini.orig \
  && sed -ri 's/^memory_limit.*/memory_limit = 256M/g' /etc/php5/fpm/php.ini \
  && sed -ri 's/^upload_max_filesize.*/upload_max_filesize = 512M/g' /etc/php5/fpm/php.ini \
  && sed -ri 's/^post_max_size.*/post_max_size = 512M/g' /etc/php5/fpm/php.ini
RUN sed -ri 's/^pm.max_children.*/pm.max_children = 50/g' /etc/php5/fpm/pool.d/www.conf

#Customize nginx settings
RUN sed -ri 's/^worker_processes.*/worker_processes 16;/g' /etc/nginx/nginx.conf \
  && sed -ri 's/^\sworker_connections.*/\tworker_connections 4096;/g' /etc/nginx/nginx.conf \
  && sed -ri 's/^\s#\smulti_accept.*/\tmulti_accept on;/g' /etc/nginx/nginx.conf

# Install Docker from Docker Inc. repositories.
RUN curl -sSL https://get.docker.com/ | sh

# Define default command.
CMD service php5-fpm start && /usr/sbin/nginx

# Expose ports.
EXPOSE 80
EXPOSE 443

FROM ubuntu:14.04

MAINTAINER Zsolt Demi <zsolt.demi@madtatu.com>

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get install -y -q apache2 libapache2-mod-php5 ;\
    a2enmod rewrite

RUN apt-get clean

ADD etc/apache2/sites-available/000-default.conf /etc/apache2/sites-available/000-default.conf

ENV DEBIAN_FRONTEND dialog

ENV APACHE_RUN_USER www-data
ENV APACHE_RUN_GROUP www-data
ENV APACHE_LOG_DIR /var/log/apache2
ENV APACHE_LOCK_DIR /var/lock/apache2
ENV APACHE_RUN_DIR /var/run/apache2
ENV APACHE_PID_FILE /var/run/apache2/apache2.pid

EXPOSE 80

ENTRYPOINT ["/usr/sbin/apache2"]
CMD ["-D", "FOREGROUND"]

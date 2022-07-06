FROM centos:6.7

#install remi
RUN yum -y install epel-release
RUN yum -y install wget
RUN wget -P /etc/yum.repos.d http://rpms.famillecollet.com/enterprise/remi.repo

#install php
RUN yum -y --enablerepo=remi,remi-php56 install php php-common

#install composer
RUN curl -sS https://getcomposer.org/installer | php; mv composer.phar /usr/local/bin/composer

#install fluentd
ADD etc/td.repo /etc/yum.repos.d/td.repo
RUN rpm --import https://packages.treasuredata.com/GPG-KEY-td-agent; yum check-update; yes|yum install -y td-agent
RUN service td-agent start; chkconfig td-agent on
ADD etc/td-agent.conf /etc/td-agent/td-agent.conf

#build application
WORKDIR /var/www
ADD Logexp ./
RUN composer install --no-dev

EXPOSE 8080

#start script
ADD start.sh /var/www/start.sh
RUN chmod +x start.sh
CMD ./start.sh

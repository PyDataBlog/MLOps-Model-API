FROM ubuntu
MAINTAINER Stephen Liang "docker-maint@stephenliang.pw"

RUN apt-get update -y && apt-get install -y php5-fpm php5-intl php-apc php5-gd php5-intl php5-mysqlnd php5-pgsql php-pear php5-cli php5-mcrypt && rm -rf /var/lib/apt/lists/*

# Listen to port 9000 instead of listening to a unix socket to enable docker to link via ports
RUN sed -i 's/\/var\/run\/php5-fpm.sock/9000/g' /etc/php5/fpm/pool.d/www.conf 

CMD php5-fpm --nodaemonize -y /etc/php5/fpm/php-fpm.conf

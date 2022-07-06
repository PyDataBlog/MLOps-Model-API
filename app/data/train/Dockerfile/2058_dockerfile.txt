FROM swodev/apache-php:dev

RUN apt-get update
RUN apt-get install -y mysql-server

RUN rm -rf /var/www
RUN ln -s /srv/application /var/www

RUN mkdir -p /tmp/app/cache /tmp/app/log
RUN chown -R www-data:www-data /tmp/app/

ADD entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

EXPOSE 80

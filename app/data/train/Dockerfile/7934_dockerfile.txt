FROM nginx:1.11.6-alpine

MAINTAINER Ramzi Double <double.ramzi@hotmail.com>

ADD nginx.conf /etc/nginx/
ADD double-gestion.project.conf /etc/nginx/conf.d/

RUN echo "upstream php-upstream { server php-fpm:9000; }" > /etc/nginx/conf.d/upstream.conf
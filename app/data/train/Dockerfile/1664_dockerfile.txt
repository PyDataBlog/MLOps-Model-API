FROM nginx
MAINTAINER Benoit TRAVERS <benoit.travers.fr@gmail.com>

COPY misc/nginx.conf /etc/nginx/conf.d/default.conf
COPY build /usr/share/nginx/html/

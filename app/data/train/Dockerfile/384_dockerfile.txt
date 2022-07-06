FROM nginx
MAINTAINER Octoblu <docker@octoblu.com>
EXPOSE 80

COPY . /usr/share/nginx/html

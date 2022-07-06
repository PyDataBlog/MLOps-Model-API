# This file describes how to build hipache into a runnable linux container with all dependencies installed
# To build:
# 1) Install docker (http://docker.io)
# 2) Clone hipache repo if you haven't already: git clone https://github.com/dotcloud/hipache.git
# 3) Build: cd hipache && docker build .
# 4) Run:
# docker run -d <imageid>
# redis-cli
#
# VERSION                0.2
# DOCKER-VERSION        0.4.0

FROM inthecloud247/kdocker-base
MAINTAINER inthecloud247 "inthecloud247@gmail.com"

RUN apt-get -y install redis-server

RUN wget -O - http://nodejs.org/dist/v0.8.26/node-v0.8.26-linux-x64.tar.gz | tar -C /usr/local/ --strip-components=1 -zxv

RUN npm install hipache -g

# copy downloaded files

ADD files/hipache.conf /etc/supervisord/conf.d/hipache.conf
ADD files/redis-server.conf /etc/supervisord/conf.d/redis-server.conf

ADD files/config.dev.json /usr/local/lib/node_modules/hipache/config/config_dev.json
ADD files/config_test.json /usr/local/lib/node_modules/hipache/config/config_test.json
ADD files/config.json /usr/local/lib/node_modules/hipache/config/config.json

EXPOSE 80
EXPOSE 6379

CMD  ["/usr/bin/supervisord", "--nodaemon"]

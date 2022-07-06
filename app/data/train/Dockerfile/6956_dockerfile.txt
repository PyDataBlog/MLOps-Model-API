#################################
#version: 1.00
#desc: opentsdb镜像
#opentsdb Version:2.3.0RC2
#################################
FROM openjdk:8-jdk-alpine
MAINTAINER truman

ENV TSDB_VERSION 2.3.0RC2
RUN mkdir /opt/
WORKDIR /opt/
RUN echo http://dl-cdn.alpinelinux.org/alpine/edge/community >> /etc/apk/repositories \
    && apk --update add \
       bash \
       wget \
    && apk add -U --no-cache gnuplot git  automake autoconf make python \
    && wget --no-check-certificate \
       -O v${TSDB_VERSION}.zip \
       https://github.com/OpenTSDB/opentsdb/archive/v${TSDB_VERSION}.zip \
    && unzip v${TSDB_VERSION}.zip \
    && rm v${TSDB_VERSION}.zip \
    && cd /opt/opentsdb-${TSDB_VERSION} \
    && ./build.sh \
    && : because of issue https://github.com/OpenTSDB/opentsdb/issues/707 \
    && : commented lines do not work. These can be uncommeted when version of \
    && : tsdb is bumped. Entrypoint will have to be updated too. \
    && : cd build \
    && : make install \
    && : cd / \
    && : rm -rf /opt/opentsdb-${TSDB_VERSION} \
    && rm -rf /var/cache/apk/*

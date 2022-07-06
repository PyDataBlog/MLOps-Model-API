FROM archlinux
MAINTAINER "Johann Weging <johann@weging.com>"

ENV DUMB_INIT_VERSION 1.0.1
ENV GOSU_VERSION 1.8

RUN set -x \
&& curl -sSL https://github.com/Yelp/dumb-init/releases/download/v$DUMB_INIT_VERSION/dumb-init_${DUMB_INIT_VERSION}_amd64 > /usr/bin/dumb-init \
&& chmod +x /usr/bin/dumb-init \
&& curl -sSL https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-amd64 > /usr/bin/gosu \
&& curl -sSL https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-amd64.asc > /tmp/gosu.asc \
&& export GNUPGHOME="$(mktemp -d)" \
&& gpg --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
&& gpg --batch --verify /tmp/gosu.asc /usr/bin/gosu \
&& chmod +x /usr/bin/gosu \
&& rm -rf "$GNUPGHOME" /tmp/*

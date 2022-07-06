FROM openjdk:8-jdk-alpine
ARG ES_VERSION=5.6.1
LABEL Description="Docker image for basic Elasticsearch service, without XPack" Vendor="FlexStudios" Version="${ES_VERSION}" authors="Kwok Yao Chim <yaochim8@gmail.com>"
ENV TZONE="Europe/London" ES_VERSION="${ES_VERSION}"
RUN apk add --update bash ca-certificates wget nodejs tzdata \
    && update-ca-certificates \
    && rm -rf /var/cache/apk/* \
    && echo "${TZONE}" > /etc/timezone \
    && ln -sf /usr/share/zoneinfo/${TZONE} /etc/localtime \
    && adduser -s /bin/bash -u 6000 -D elasticsearch \
    && mkdir /opt \
    && wget -O /opt/elasticsearch-${ES_VERSION}.tar.gz https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-${ES_VERSION}.tar.gz \
    && tar -C /opt -xzf /opt/elasticsearch-${ES_VERSION}.tar.gz \
    && mv /opt/elasticsearch-${ES_VERSION} /opt/elasticsearch \
    && mkdir /opt/elasticsearch/data /opt/elasticsearch/logs \
    && chown -R elasticsearch:elasticsearch /opt/elasticsearch
USER elasticsearch
EXPOSE 9200 9300
WORKDIR /opt/elasticsearch
ENTRYPOINT ["/opt/elasticsearch/bin/elasticsearch"]


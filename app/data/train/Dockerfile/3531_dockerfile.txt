FROM openjdk:8-jre-alpine
MAINTAINER Marcelo Barbosa <infobarbosa@yahoo.com.br>

ARG MIRROR=http://apache.mirrors.pair.com
ARG VERSION=3.4.10

LABEL name="zookeeper" version=$VERSION

RUN apk add --no-cache wget bash \
    && mkdir /opt \
    && wget -q -O - $MIRROR/zookeeper/zookeeper-$VERSION/zookeeper-$VERSION.tar.gz | tar -xzf - -C /opt \
    && mv /opt/zookeeper-$VERSION /opt/zookeeper \
    && cp /opt/zookeeper/conf/zoo_sample.cfg /opt/zookeeper/conf/zoo.cfg \
    && mkdir -p /tmp/zookeeper \
    && mkdir -p /opt/zookeeper/data \
    && mkdir -p /var/zookeeper/logs

EXPOSE 2181 2888 3888

WORKDIR /opt/zookeeper

ENTRYPOINT ["bin/zkServer.sh"]
CMD ["start-foreground"]


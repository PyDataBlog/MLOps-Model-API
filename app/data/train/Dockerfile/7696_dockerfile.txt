FROM tomcat:8-jre8

ENV APR_VERSION 1.5.2

# ################################################################################ APR
RUN apt-get update
RUN apt-get install -yq build-essential openssl libssl-dev libtcnative-1

RUN cd /tmp \
    && curl -fSL "http://ftp.cixug.es/apache/apr/apr-$APR_VERSION.tar.gz" -O \
    && tar xvzf apr-$APR_VERSION.tar.gz && cd apr-$APR_VERSION \
    && ./configure --prefix=/usr && make && make install
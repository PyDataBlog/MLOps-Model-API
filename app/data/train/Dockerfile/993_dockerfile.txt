FROM debian:8
MAINTAINER dnscrypt.io

ENV DEBIAN_FRONTEND=noninteractive

ENV BUILD_DEPENDENCIES \
    autoconf \
    bzip2 \
    curl \
    gcc \
    make

RUN set -x && \
    apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y libssl-dev libcap-dev $BUILD_DEPENDENCIES

ENV BIND_VERSION 9.10.5-P1
ENV BIND_SHA256 82fb885de927fdb4db0a0bb5e5efda839a857ff70adbcfcb0486a010924ae5cd
ENV BIND_DOWNLOAD_URL http://ftp.isc.org/isc/bind9/${BIND_VERSION}/bind-${BIND_VERSION}.tar.gz
ENV BIND_USER named

RUN set -x && \
    mkdir -p /tmp/src && \
    cd /tmp/src && \
    curl -sSL $BIND_DOWNLOAD_URL -o bind.tar.gz && \
    echo "${BIND_SHA256} *bind.tar.gz" | sha256sum -c - && \
    tar xzf bind.tar.gz && \
    rm -f bind.tar.gz && \
    cd bind-${BIND_VERSION} && \
    ./configure --enable-threads --with-openssl=yes --with-libtool --prefix=/opt/bind && \
    make && make install && \
    ldconfig -v && \
    groupadd $BIND_USER && \
    useradd -d /var/bind -g $BIND_USER -s /bin/false $BIND_USER && \
    rm -rf /tmp/* /var/tmp/*

COPY config/* /opt/bind/etc/

RUN set -x && \
    chown -R ${BIND_USER}:${BIND_USER} /opt/bind && \
    apt-get autoremove -y && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

EXPOSE 53/udp 53/tcp

ENTRYPOINT ["/opt/bind/sbin/named", "-4", "-c/opt/bind/etc/named.conf", "-g", "-unamed"]

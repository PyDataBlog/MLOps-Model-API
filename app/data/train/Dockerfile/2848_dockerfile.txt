FROM ubuntu:14.04.1

ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update
RUN apt-get install -y libssl-dev libpcre3-dev wget build-essential
WORKDIR /root
RUN wget  http://www.haproxy.org/download/1.5/src/haproxy-1.5.11.tar.gz
RUN tar zxvf haproxy-1.5.11.tar.gz
WORKDIR /root/haproxy-1.5.11


RUN make TARGET=linux2628 USE_OPENSSL=yes USE_STATIC_PCRE=1
#RUN make install
RUN mkdir -p /dist/lib /dist/usr/sbin /dist/etc/haproxy/errors
RUN cp haproxy /dist/usr/sbin
RUN cp haproxy-systemd-wrapper /dist/usr/sbin
RUN cp /lib/x86_64-linux-gnu/libcrypt.so.1 /dist/lib
RUN cp /lib/x86_64-linux-gnu/libssl.so.1.0.0 /dist/lib
RUN cp /lib/x86_64-linux-gnu/libcrypto.so.1.0.0 /dist/lib
RUN cp -r examples/errorfiles/*.http /dist/etc/haproxy/errors/

WORKDIR /dist
RUN tar cf /overlayfs.tar .

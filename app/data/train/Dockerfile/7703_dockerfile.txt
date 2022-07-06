FROM progrium/busybox
MAINTAINER Pavel Litvinenko "gerasim13@gmail.com"
RUN opkg-install curl bash git sshtunnel
ENTRYPOINT ["git"]
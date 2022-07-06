# Dockerfile for an ArchLinux image containing MongoDB.
# Starts mongod instance by default.
#
# Licensed under MIT License, for details see 'LICENSE'.

FROM ygrenzinger/archlinux-arm
MAINTAINER ralph.greschner.dev@gmail.com

RUN pacman -Syyu --noconfirm; pacman-db-upgrade;
RUN pacman -S mongodb --noconfirm; mkdir -p /data/db;
ADD run.sh /run.sh
RUN chmod a+x /run.sh

VOLUME ["/data"]
EXPOSE 27017

CMD ["/run.sh"]

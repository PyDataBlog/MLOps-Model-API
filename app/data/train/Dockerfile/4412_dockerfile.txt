FROM ouyi/docker-ubuntu-precise-core-i386
MAINTAINER Yi Ou <https://github.com/ouyi>
ENV DEBIAN_FRONTEND noninteractive

RUN echo "nameserver 8.8.8.8" | tee /etc/resolv.conf > /dev/null
RUN apt-get -y update
RUN apt-get -y upgrade
RUN apt-get -y install iputils-ping firefox openjdk-7-jre icedtea-7-plugin libpango1.0-0 libxft2 libxmu6 libxv1 fonts-takao

RUN apt-get clean
RUN rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

CMD ["/usr/bin/firefox"]

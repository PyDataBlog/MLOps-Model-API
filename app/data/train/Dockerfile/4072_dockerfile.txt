FROM vivaserver/wheezy
MAINTAINER cristian.arroyo@vivaserver.com

RUN echo "deb http://http.debian.net/debian wheezy-backports main" >> /etc/apt/sources.list
RUN apt-get -qq update
RUN apt-get -qq install ansible
RUN echo "localhost" >> /etc/ansible/hosts
RUN apt-get -qq clean

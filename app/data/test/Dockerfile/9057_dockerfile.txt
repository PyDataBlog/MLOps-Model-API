FROM ubuntu:14.04
MAINTAINER Mattias Lundberg <lundberg.mattias@gmail.com>

ENV LANG en_US.UTF-8
RUN locale-gen $LANG
RUN echo "deb http://archive.ubuntu.com/ubuntu/ trusty multiverse" >> /etc/apt/sources.list
RUN apt-get update && \
	apt-get install -y snmpd snmp-mibs-downloader && \
	apt-get clean

RUN echo "createUser -e 80000001 myuser MD5 mypassword DES myprivacy" >> /etc/snmp/snmptrapd.conf
RUN echo "authUser log myuser" >> /etc/snmp/snmptrapd.conf

EXPOSE 161
EXPOSE 162

ADD ./start.sh /start.sh
CMD ["/start.sh"]

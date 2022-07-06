FROM busybox:ubuntu-14.04

MAINTAINER tsaikd <tsaikd@gmail.com>

ENV FILEBEAT_VERSION 5.0.1

ADD filebeat-${FILEBEAT_VERSION}-linux-x86_64.tar.gz /usr/local

RUN mv /usr/local/filebeat-${FILEBEAT_VERSION}-linux-x86_64 /filebeat

WORKDIR /filebeat

CMD ["./filebeat", "-e"]

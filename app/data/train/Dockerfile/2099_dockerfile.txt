FROM centos:centos7

MAINTAINER John Gasper <jgasper@unicon.net>

ENV JAVA_HOME=/opt/openjdk7

RUN java_version=8.0.172; \ 
    zulu_version=8.30.0.1; \   
    yum update -y \
    && yum install -y wget tar \
    && yum clean all \
    \
    && echo 'Downloading the OpenJDK Zulu...' \ 
    && wget -q http://cdn.azul.com/zulu/bin/zulu$zulu_version-jdk$java_version-linux_x64.tar.gz \
    && echo "0a101a592a177c1c7bc63738d7bc2930  zulu$zulu_version-jdk$java_version-linux_x64.tar.gz" | md5sum -c - \
    && tar -zxvf zulu$zulu_version-jdk$java_version-linux_x64.tar.gz -C /opt \
    && ln -s /opt/zulu$zulu_version-jdk$java_version-linux_x64 $JAVA_HOME \
    && rm zulu$zulu_version-jdk$java_version-linux_x64.tar.gz

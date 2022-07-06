FROM centos:7

MAINTAINER Justin R. Stout <justinrstout@justinrstout.com>

ENV JAVA_VERSION 8
ENV JAVA_UPDATE 60
ENV JAVA_BUILD 27

RUN yum install -y wget

RUN wget --no-cookies --no-check-certificate --header \
    "Cookie: gpw_e24=htt%3A%2F%2Fwww.oracle.com%2F; oraclelicense=accept-securebackup-cookie" \
    "http://download.oracle.com/otn-pub/java/jdk/${JAVA_VERSION}u${JAVA_UPDATE}-b${JAVA_BUILD}/jdk-${JAVA_VERSION}u${JAVA_UPDATE}-linux-x64.rpm"

RUN yum remove -y wget

RUN yum localinstall -y jdk-${JAVA_VERSION}u${JAVA_UPDATE}-linux-x64.rpm

RUN rm jdk-${JAVA_VERSION}u${JAVA_UPDATE}-linux-x64.rpm

RUN alternatives --install /usr/bin/java java /usr/java/jdk1.${JAVA_VERSION}.0_${JAVA_UPDATE}/bin/java 1
RUN alternatives --install /usr/bin/javac javac /usr/java/jdk1.${JAVA_VERSION}.0_${JAVA_UPDATE}/bin/javac 1
RUN alternatives --install /usr/bin/jar jar /usr/java/jdk1.${JAVA_VERSION}.0_${JAVA_UPDATE}/bin/jar 1

ENV JAVA_HOME /usr/java/jdk1.${JAVA_VERSION}.0_${JAVA_UPDATE}

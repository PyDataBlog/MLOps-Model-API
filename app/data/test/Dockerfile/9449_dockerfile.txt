FROM tianon/centos:6.5
MAINTAINER Jasmine Hegman <jasmine@webpt.com>


# Install dependencies
RUN yum -y update
RUN yum -y install bridge-utils wget dnsmasq build-essential tar


# Install appropriate JDK
RUN yum -y install java-1.7.0-openjdk.x86_64


# Install Druid

WORKDIR /root/druid/

RUN curl http://static.druid.io/artifacts/releases/druid-services-0.6.121-bin.tar.gz -o druid-services-0.6.121-bin.tar.gz
RUN tar -vxf druid-services-*-bin.tar.gz
RUN rm -rf druid-services-*-bin.tar.gz

WORKDIR /root/druid/druid-services-0.6.121/

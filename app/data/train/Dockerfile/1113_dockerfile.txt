FROM centos:centos7

MAINTAINER Alfredo Matas "amatas@gmail.com"

RUN yum -y install java-1.7.0-openjdk-headless && \
    yum clean all

# Define default command.
CMD ["/bin/bash"]

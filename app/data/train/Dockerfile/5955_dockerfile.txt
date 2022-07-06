FROM fedora:21

MAINTAINER docker@syssolhk.com

RUN yum -y update && yum clean all

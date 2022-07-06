#------------------------------------------------------------------------------
# Set the base image for subsequent instructions:
#------------------------------------------------------------------------------

FROM centos:7
MAINTAINER Marc Villacorta Morera <marc.villacorta@gmail.com>

#------------------------------------------------------------------------------
# Install:
#------------------------------------------------------------------------------

RUN rpm --import http://mirror.centos.org/centos/7/os/x86_64/RPM-GPG-KEY-CentOS-7 && \
    rpm --import http://yum.puppetlabs.com/RPM-GPG-KEY-puppetlabs && \
    rpm --import http://dl.fedoraproject.org/pub/epel/RPM-GPG-KEY-EPEL-7 && \
    rpm --import http://yum-repositories.s3-website-eu-west-1.amazonaws.com/RPM-GPG-KEY-booddies && \
    yum update -y && yum clean all

RUN yum install -y epel-release \
    http://yum.puppetlabs.com/puppetlabs-release-el-7.noarch.rpm \
    http://yum-repositories.s3-website-eu-west-1.amazonaws.com/booddies/booddies-release-1-1.noarch.rpm && \
    yum clean all

RUN yum install -y dhclient httpd wget rsync yum-utils createrepo repoview && \
    rm -f /etc/httpd/conf.d/welcome.conf && \
    yum clean all

ADD rootfs /

#------------------------------------------------------------------------------
# Entrypoint:
#------------------------------------------------------------------------------

ENTRYPOINT ["/init", "/usr/sbin/httpd", "-DFOREGROUND"]

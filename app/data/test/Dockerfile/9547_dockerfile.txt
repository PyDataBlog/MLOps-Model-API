FROM ubuntu:12.04
MAINTAINER voxsim-mala "https://github.com/malavolti/docker-puppetagent"
ENV DEBIAN_FRONTEND noninteractive

RUN echo "deb http://archive.ubuntu.com/ubuntu trusty main universe multiverse" > /etc/apt/sources.list
RUN echo "#!/bin/sh\nexit 0" > /usr/sbin/policy-rc.d
RUN apt-get update
RUN apt-get install -y wget
RUN wget -q https://apt.puppetlabs.com/puppetlabs-release-trusty.deb -O /tmp/puppetlabs.deb
RUN dpkg -i /tmp/puppetlabs.deb
RUN apt-get update
RUN apt-get -y install puppet ruby augeas-tools  

ADD puppet.conf /etc/puppet/puppet.conf

EXPOSE 22
EXPOSE 3000

CMD ["/bin/bash"]

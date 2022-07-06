FROM ubuntu:14.04
MAINTAINER Niko Kiuru "niko@kiuru.net"

RUN locale-gen en_US.UTF-8
RUN echo "export LC_ALL=en_US.UTF-8" >> ~/.bashrc
RUN export LC_ALL=en_US.UTF-8

RUN apt-get update && apt-get -y install ruby ruby-dev make git
RUN gem install --no-ri --no-rdoc puppet librarian-puppet deep_merge

# https://ask.puppetlabs.com/question/10955/issues-with-ubuntu-docker-image-and-puppetlabs-mysql/?answer=15403#post-id-15403
RUN /bin/echo -e '#!/bin/sh\nexit 0' > /usr/sbin/policy-rc.d

FROM ubuntu:14.04
MAINTAINER hamsterksu@gmail.com

RUN apt-get update
RUN apt-get install -y software-properties-common 

# Install Java.
RUN \
  echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections && \
  add-apt-repository -y ppa:webupd8team/java && \
  apt-get update && \
  apt-get install -y oracle-java8-installer && \
  rm -rf /var/cache/oracle-jdk8-installer

# Define commonly used JAVA_HOME variable
ENV JAVA_HOME /usr/lib/jvm/java-8-oracle

RUN apt-get install -y git vim wget maven
RUN apt-get install -y curl

RUN curl -fsSL https://get.docker.com/ | sh

RUN echo "set tabstop=4 shiftwidth=4 expandtab" >> ~/.vimrc

CMD bash

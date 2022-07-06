# A cutting edge JEE development environment using:
#	GVM - for groovy/grails environment & SDK management
# 	Grails - the latest version at time of installation, thanks to GVM
# 	Oracle JDK 8
# 	Tomcat 7
# 	IntelliJ - Ultimate, 30-day free trial of version 13

FROM dockerfile/ubuntu-desktop

MAINTAINER Alex Collins, alexcollins.org

# add-apt-repository so we can install JDK8
RUN apt-get install -y software-properties-common

RUN add-apt-repository -y ppa:webupd8team/java

RUN apt-get update

# Trick the JDK install into accepting the license
RUN echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections

RUN echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections

RUN apt-get install -y oracle-java8-installer tomcat7 curl unzip tar openssh-server

RUN apt-get install -y python-pip

RUN easy_install supervisor

# System setup complete, now create dev environment

RUN yes | adduser dev

ADD ./install-grails.sh /home/dev/install-grails.sh

ADD ./supervisord.conf /home/dev/supervisord.conf

ADD ./vncpasswd /home/dev/.vnc/passwd

RUN chown dev:dev -R /home/dev/.vnc/

RUN chmod 400 /home/dev/.vnc/passwd

USER dev

ENV HOME /home/dev

WORKDIR /home/dev/

RUN curl -s get.gvmtool.net | bash

RUN wget http://download.jetbrains.com/idea/ideaIU-13.1.2.tar.gz -O ~/idea.tar.gz

RUN tar zxf idea.tar.gz

#ENTRYPOINT /bin/bash
ENTRYPOINT supervisord -n -c /home/dev/supervisord.conf

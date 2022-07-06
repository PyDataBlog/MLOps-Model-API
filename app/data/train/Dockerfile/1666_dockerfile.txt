FROM steven/ubuntu-ssh

MAINTAINER steven

ADD jdk-8u144-linux-x64.tar.gz /usr/lib/jvm
ENV JAVA_HOME /usr/lib/jvm/jdk1.8.0_144
ENV PATH $JAVA_HOME/bin:$PATH

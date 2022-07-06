FROM debian
MAINTAINER Joshua Krage <jkrage@guisarme.us>

# Modified from:
# http://www.webupd8.org/2014/03/how-to-install-oracle-java-8-in-debian.html
RUN apt-get update \
    && apt-get -y install curl gnupg \
    && echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections \
    && echo "deb http://ppa.launchpad.net/webupd8team/java/ubuntu trusty main" | tee /etc/apt/sources.list.d/webupd8team-java.list \
    && echo "deb-src http://ppa.launchpad.net/webupd8team/java/ubuntu trusty main" | tee -a /etc/apt/sources.list.d/webupd8team-java.list \
    && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys EEA14886 \
    && apt-get update \
    && apt-get -y install oracle-java8-installer \
                          oracle-java8-set-default \
                          oracle-java8-unlimited-jce-policy \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /var/cache/oracle-jdk8-installer

ENV JAVA_HOME=/usr/lib/jvm/java-8-oracle

CMD ["/bin/bash"]

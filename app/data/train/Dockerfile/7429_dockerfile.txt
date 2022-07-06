FROM inthecloud247/kdocker-base
MAINTAINER inthecloud247 "inthecloud247@gmail.com"

# openjdk7
RUN apt-get -y install --no-install-recommends openjdk-7-jdk

# set JAVA_HOME
ENV JAVA_HOME /usr/lib/jvm/java-7-openjdk-amd64

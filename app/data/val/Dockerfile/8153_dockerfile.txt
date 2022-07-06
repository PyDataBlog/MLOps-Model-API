FROM dockerfile/java
MAINTAINER Gian Inductivo <gian@dynamicobjx.com>

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y --force-yes software-properties-common wget

RUN wget http://apt.typesafe.com/repo-deb-build-0002.deb
RUN dpkg -i repo-deb-build-0002.deb
RUN rm -f repo-deb-build-0002.deb

RUN apt-get update

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y --force-yes openjdk-6-jre libjansi-java
RUN wget http://www.scala-lang.org/files/archive/scala-2.10.3.deb
RUN dpkg -i scala-2.10.3.deb
RUN rm -f scala-2.10.3.deb

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y --force-yes sbt
RUN wget http://repo.scala-sbt.org/scalasbt/sbt-native-packages/org/scala-sbt/sbt/0.13.1/sbt.deb
RUN dpkg -i sbt.deb
RUN rm -f sbt.deb

RUN update-alternatives --set java /usr/lib/jvm/java-7-oracle/jre/bin/java

ADD ./ /opt/app/
WORKDIR /opt/app
RUN sbt clean compile stage
EXPOSE 8080

ENTRYPOINT ["/opt/app/target/start", "Rest"]


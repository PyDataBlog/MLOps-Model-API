FROM debian:jessie

MAINTAINER Jan Boonen <jan.boonen@geodan.nl>

ENV JAVA_VERSION_MAJOR 8
ENV JAVA_VERSION_MINOR 66
ENV JAVA_VERSION_BUILD 17
ENV JAVA_PACKAGE jdk

ENV JAVA_HOME /opt/jdk

# Define en_US.
ENV LANGUAGE en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LC_CTYPE en_US.UTF-8
ENV LC_MESSAGES en_US.UTF-8
ENV LC_ALL  en_US.UTF-8

ADD assets/aliases /root/
ADD assets/post_install.sh /root/
ADD assets/dynamic_memory_options.sh /java/

# Install curl, locales, apt-utils and Oracle JDK
# create en_US.UTF-8
# update distribution package
RUN apt-get update -qq && \
  apt-get install -y -qq apt-utils curl locales && \
  apt-get upgrade -y -qq && \
  sed -i 's/^# en_US.UTF-8 UTF-8$/en_US.UTF-8 UTF-8/g' /etc/locale.gen && locale-gen && \
  update-locale LANG=$LANGUAGE LC_ALL=$LC_ALL && \
  chmod +x /root/post_install.sh /java/dynamic_memory_options.sh && \
  mv /root/aliases /root/.aliases && \
  echo "source ~/.aliases" >> /root/.bashrc && \
  /root/post_install.sh

RUN mkdir -p /opt && \
  curl -jksSLH "Cookie: oraclelicense=accept-securebackup-cookie" \
    http://download.oracle.com/otn-pub/java/jdk/${JAVA_VERSION_MAJOR}u${JAVA_VERSION_MINOR}-b${JAVA_VERSION_BUILD}/${JAVA_PACKAGE}-${JAVA_VERSION_MAJOR}u${JAVA_VERSION_MINOR}-linux-x64.tar.gz \
    | tar -xzf - -C /opt && \
  ln -s /opt/jdk1.${JAVA_VERSION_MAJOR}.0_${JAVA_VERSION_MINOR} /opt/jdk && \
  rm -rf /opt/jdk/*src.zip \
    /opt/jdk/lib/missioncontrol \
    /opt/jdk/lib/visualvm \
    /opt/jdk/lib/*javafx* \
    /opt/jdk/jre/lib/plugin.jar \
    /opt/jdk/jre/lib/ext/jfxrt.jar \
    /opt/jdk/jre/bin/javaws \
    /opt/jdk/jre/lib/javaws.jar \
    /opt/jdk/jre/lib/desktop \
    /opt/jdk/jre/plugin \
    /opt/jdk/jre/lib/deploy* \
    /opt/jdk/jre/lib/*javafx* \
    /opt/jdk/jre/lib/*jfx* \
    /opt/jdk/jre/lib/amd64/libdecora_sse.so \
    /opt/jdk/jre/lib/amd64/libprism_*.so \
    /opt/jdk/jre/lib/amd64/libfxplugins.so \
    /opt/jdk/jre/lib/amd64/libglass.so \
    /opt/jdk/jre/lib/amd64/libgstreamer-lite.so \
    /opt/jdk/jre/lib/amd64/libjavafx*.so \
    /opt/jdk/jre/lib/amd64/libjfx*.so

# Add /srv/java on PATH variable
ENV PATH ${PATH}:${JAVA_HOME}/bin:/java

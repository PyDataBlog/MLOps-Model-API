FROM ubuntu:18.04 AS shell-ubuntu
LABEL version='18.04'
LABEL author='Jongho Kim'

ENV DEBIAN_FRONTEND noninteractive

COPY .bashrc /root
COPY .Xmodmap /root

RUN apt-get update; apt-get install -y --no-install-recommends apt-utils; apt-get install -y rlwrap openjdk-8-jdk openjdk-11-jdk

################################################################################
#                                                                              #
#    Environment variables                                                     #
#                                                                              #
################################################################################

# Terminal
ENV TERM xterm-256color

# Tibero6
ENV TB_HOME /etc/tibero/tibero6
ENV TB_SID tibero
ENV TB_PROF_DIR ${TB_HOME}/bin/prof
ENV PATH ${PATH}:${TB_HOME}/bin:${TB_HOME}/client/bin
ENV LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${TB_HOME}/client/lib
ENV C_INCLUDE_PATH ${C_INCLUDE_PATH}:${TB_HOME}/include:${TB_HOME}/client/include

# Java
ENV JAVA8_HOME  /usr/lib/jvm/java-8-openjdk-amd64
ENV JAVA11_HOME /usr/lib/jvm/java-11-openjdk-amd64
ENV JAVA_HOME   ${JAVA11_HOME}


################################################################################
#                                                                              #
#    Aliases                                                                   #
#                                                                              #
################################################################################

# Emacs
RUN echo 'alias emacs="emacs -nw"' >> /root/.bashrc

# Tibero6
RUN echo 'alias tbsql="rlwrap tbsql"' >> /root/.bashrc

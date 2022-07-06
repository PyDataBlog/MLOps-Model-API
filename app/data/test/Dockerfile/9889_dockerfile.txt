FROM damm/ubuntu:14.04
MAINTAINER <scott@likens.us>
RUN apt-mark hold initscripts udev plymouth mountall
RUN sed -i.bak 's/main$/main universe/' /etc/apt/sources.list
RUN apt-get update
RUN dpkg-divert --local --rename --add /sbin/initctl
RUN apt-get update && apt-get -y install software-properties-common wget curl
RUN add-apt-repository ppa:webupd8team/java
RUN apt-get update 
RUN echo oracle-java7-installer shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections
RUN apt-get -y install oracle-java7-installer && apt-get clean
RUN update-alternatives --display java
RUN echo "JAVA_HOME=/usr/lib/jvm/java-7-oracle" >> /etc/environment
RUN echo '# /lib/init/fstab: cleared out for bare-bones lxc' > /lib/init/fstab
RUN ln -sf /proc/self/mounts /etc/mtab

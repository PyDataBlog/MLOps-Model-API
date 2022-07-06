# The development environment for 
#  - Android (from Gingerbread to KitKat).
#  - OpenWrt
#  - Buildroot
#
# This container is based on ubuntu:10.04

FROM ubuntu:10.04

MAINTAINER Evans Lai <evanslai@gmail.com>

# Install the basic utilities
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y \
		vim exuberant-ctags cscope subversion zip

ADD vimrc /.vimrc
RUN mkdir /.vim
ADD taglist_46.zip /taglist_46.zip
RUN unzip taglist_46.zip -d /.vim
WORKDIR /.vim/doc
RUN vim -c q "helptags ."
WORKDIR /
RUN rm -f taglist_46.zip
RUN cp -dRf /.vimrc /.vim /root/

# Install the Git 
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y \
		libssl-dev libcurl4-openssl-dev libexpat1-dev \
		perl tcl gettext

RUN mkdir /build
ADD git-master.zip /build/git-master.zip
WORKDIR /build
RUN unzip git-master.zip
WORKDIR /build/git-master
RUN make prefix=/usr install
WORKDIR /

# Setup Android development
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y \
		gnupg flex bison gperf build-essential \
		zip curl zlib1g-dev libc6-dev lib32ncurses5-dev ia32-libs \
		x11proto-core-dev libx11-dev lib32readline5-dev lib32z-dev \
		libgl1-mesa-dev g++-multilib mingw32 tofrodos python-markdown \
		libxml2-utils xsltproc

RUN ln -s /usr/lib32/mesa/libGL.so.1 /usr/lib32/mesa/libGL.so
RUN curl https://storage.googleapis.com/git-repo-downloads/repo > /usr/bin/repo
RUN chmod a+x /usr/bin/repo

# Install SUN Java 6
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y python-software-properties
RUN echo "deb http://archive.canonical.com/ lucid partner" >> /etc/apt/sources.list
RUN add-apt-repository ppa:ferramroberto/java
RUN apt-get update
RUN echo sun-java6-jdk shared/accepted-sun-dlj-v1-1 select true | /usr/bin/debconf-set-selections
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y sun-java6-jdk


# Install packages for other build systems
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y \
		texinfo wget gawk rsync

# Install the SSH Server
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y dropbear
RUN echo 'root:root' |chpasswd

# misc
RUN mkdir /root/projects
RUN echo 'export PATH="/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin"' >> /root/.bashrc

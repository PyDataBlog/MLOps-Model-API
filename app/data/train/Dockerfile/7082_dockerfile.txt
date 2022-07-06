FROM phusion/baseimage:0.9.17

RUN apt-get update
RUN apt-get install -y wget python build-essential

RUN wget https://nodejs.org/download/release/v0.12.7/node-v0.12.7.tar.gz --no-check-certificate
RUN tar zxvf node-v0.12.7.tar.gz
RUN cd node-v0.12.7 && ./configure && make && make install

RUN ln -s /usr/local/bin/node /usr/bin/node
RUN ln -s /usr/local/bin/node /usr/bin/nodejs

RUN apt-get remove -y manpages-dev patch python build-essential \
binutils build-essential cpp cpp-4.8 dpkg-dev fakeroot g++ g++-4.8 gcc \
gcc-4.8 libalgorithm-diff-perl libalgorithm-diff-xs-perl \
libalgorithm-merge-perl libasan0 libatomic1 libc-dev-bin libc6-dev \
libcloog-isl4 libdpkg-perl libfakeroot libfile-fcntllock-perl libgcc-4.8-dev \
libgmp10 libgomp1 libisl10 libitm1 libmpc3 libmpfr4 libquadmath0 \
libstdc++-4.8-dev libtimedate-perl libtsan0 linux-libc-dev make manpages \


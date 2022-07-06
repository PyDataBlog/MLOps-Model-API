FROM        centos:centos7
MAINTAINER  Marian Sievers <info@mariansievers.de>

# EXPOSE      8080

# Install software
RUN         yum -y update && yum clean all
RUN         yum install -y tar gzip wget && yum clean all

# Install io.js
RUN         mkdir /root/iojs
RUN         wget https://iojs.org/dist/v1.1.0/iojs-v1.1.0-linux-x64.tar.gz
RUN         tar -zxvf iojs-v1.1.0-linux-x64.tar.gz -C /root/iojs
RUN         rm -f iojs-v1.1.0-linux-x64.tar.gz

# add io.js to path
RUN         echo "PATH=$PATH:/root/iojs/iojs-v1.1.0-linux-x64/bin" >> /root/.bashrc

# go to /src
WORKDIR     /src/KommunalCrowd

CMD         /root/iojs/iojs-v1.1.0-linux-x64/bin/node app.js

FROM alledia/codeception

MAINTAINER Alledia <suport@alledia.com>


# Fix the policy-rc.d
RUN \
    echo "#!/bin/sh" > /usr/sbin/policy-rc.d && \
    echo "exit 0" >> /usr/sbin/policy-rc.d

# Commands
RUN \
    echo "deb http://us-east-1.ec2.archive.ubuntu.com/ubuntu/ saucy multiverse" | tee -a /etc/apt/sources.list && \
    echo "deb-src http://us-east-1.ec2.archive.ubuntu.com/ubuntu/ saucy multiverse" | tee -a /etc/apt/sources.list && \
    echo "deb http://us-east-1.ec2.archive.ubuntu.com/ubuntu/ saucy-updates multiverse" | tee -a /etc/apt/sources.list && \
    echo "deb-src http://us-east-1.ec2.archive.ubuntu.com/ubuntu/ saucy-updates multiverse" | tee -a /etc/apt/sources.list && \
    apt-get update && \
    echo ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true | debconf-set-selections && \
    apt-get upgrade -y && \
    apt-get install -y \
        libfreetype6 \
        libfontconfig \
        ttf-mscorefonts-installer \
        phantomjs && \
    apt-get autoremove -y && \
    apt-get clean all

from   ubuntu:14.04
from   ubuntu:14.04
run     locale-gen en_US.UTF-8
env     LANG en_US.UTF-8
env     LANGUAGE en_US:en
env     LC_ALL en_US.UTF-8
run    apt-get update
run    DEBIAN_FRONTEND=noninteractive apt-get install wget curl apt-transport-https -y --force-yes
run    wget http://github.com/leandrosouza/basebuilder/tarball/master -O basebuilder.tar.gz --no-check-certificate
run    mkdir /var/lib/tsuru
run    tar -xvf basebuilder.tar.gz -C /var/lib/tsuru --strip 1
run    cp /var/lib/tsuru/nodejs/deploy /var/lib/tsuru
run    cp /var/lib/tsuru/base/start /var/lib/tsuru
run    curl -sL https://deb.nodesource.com/setup_6.x | sudo -E bash -
run    apt-get install -y nodejs --force-yes
run    apt-get update
run    /var/lib/tsuru/base/install
run    npm install -g gulp bower node-gyp
run    ln -sf /usr/share/zoneinfo/America/Sao_Paulo /etc/localtime
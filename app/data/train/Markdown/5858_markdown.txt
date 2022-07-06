zpx-get
=======

ZPX-GET  FOR ZPANEL FR Team Installer

Detect apt-get or yum

for install

wget https://raw.github.com/ZPanelFR/zpx-get/master/zpx-get -P /usr/bin

chmod +x /usr/bin/zpx-get

usage

update depots list

zpx-get -y update

install package

zpx-get -y install package

remove package

zpx-get -y remove package

purge or erase package

zpx-get -y purge package

upgrade or dist-upgrade

zpx-get -y upgrade

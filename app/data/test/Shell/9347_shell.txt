#!/bin/bash
mkdir /home/$USER/.config/clyrics
if [ "$EUID" -ne 0 ]
  then echo "Cannot detect root! Re-run as superuser"
  exit
fi
git clone https://github.com/trizen/clyrics.git /opt/clyrics
apt install cpanminus
cpanm WWW::Mechanize
cp /opt/clyrics/plugins/*.pl /home/$USER/.config/clyrics
ln -s /opt/clyrics/clyrics /usr/local/bin/clyrics

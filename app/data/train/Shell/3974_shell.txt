#!/bin/sh

if [ `id -u` -eq 0 ]
then
  systemctl stop log2ram
  systemctl disable log2ram
  rm /etc/systemd/system/log2ram.service
  rm /usr/local/bin/log2ram
  rm /etc/cron.daily/log2ram
  echo "Removed log2ram"
else
  echo "You need to be ROOT (sudo can be used)"
fi

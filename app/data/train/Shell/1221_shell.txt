#!/bin/bash


# Author : dark-Amateur
# Date : 2013-12-11
# Version : 0.1


#
log=
log_time=
IP=
date=
now=
inotifywait=/usr/bin/inotifywait


#

while $inotifywait -q -e modify $log > /dev/null;
do
  log_time=$(grep 'Failed password' $log | awk '{print $2"-"$3}' | tail -n1 | cut -d':' -f1,2)
  now=$(date '+%d-%H:%M')
  
  if [ "$now" == "$log_time" ]; then
    IP=$(grep 'Failed password' $log | tail -n1 | sed 's/^.*from //g' | sed '/port.*$//g')
    echo $IP
  fi
done



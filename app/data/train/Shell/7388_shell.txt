#!/bin/sh
unset HISTFILE;IPADDRESS=$(/sbin/ifconfig eth0 | sed -n 's/.*inet addr:\([^ ]*\).*/\1/p');echo -e 'The current IP address for your system is: '$IPADDRESS''| mailx -s 'Ip address' srinivas@solancer.com

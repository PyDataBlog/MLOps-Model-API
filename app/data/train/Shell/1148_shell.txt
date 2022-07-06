#!/bin/bash

fqdn_pre="basic.internal.hadoop."
fqdn_post=".scloud.letv.com"

for ip in  `cat ../ip-list-cloud.txt`

do
  
  echo "IP is : $ip"

  rsync -avz /var/lib/ambari-agent/ambari-env.sh  root@$ip:/var/lib/ambari-agent/ambari-env.sh

done

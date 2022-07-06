#!/bin/bash

# Increase the yum timeout because some repos take a bit longer
grep -v "timeout" /etc/yum.conf > templist && mv templist /etc/yum.conf
echo 'timeout=999' >> /etc/yum.conf

# Update the box
yum --enablerepo=updates clean metadata
yum -y update
yum upgrade

# Install 
yum -y install epel-release 
yum -y install facter
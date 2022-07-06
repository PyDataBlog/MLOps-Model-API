#!/usr/bin/env bash

sudo apt-get update
sudo apt-get -y install curl git vim
git clone https://github.com/openstack-dev/devstack.git -b stable/icehouse devstack/
cd devstack/
wget https://gist.githubusercontent.com/everett-toews/3cbbf4c3d059848ac316/raw/6a18ded87190c87ac981e8682b783bb25aa9e844/local.conf
./stack.sh

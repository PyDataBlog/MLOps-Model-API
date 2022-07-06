#!/bin/bash -ex
go get -u github.com/mitchellh/gox
mkdir -p /root/go/src/github.com/mitchellh/
git clone https://github.com/mitchellh/packer.git /root/go/src/github.com/mitchellh/packer
cd /root/go/src/github.com/mitchellh/packer; make updatedeps; make; make dev

mkdir -p /dist
WORKSPACE=/dist
cd /root/go/bin; zip -9 ${WORKSPACE}/packer-latest-`date +%Y%m%d%H%M%S`.zip packer*

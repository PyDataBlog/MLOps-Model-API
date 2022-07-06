#!/usr/bin/env bash

git clone https://github.com/robert0714/books-ms-centos.git

set -e

cd books-ms-centos

docker build \
    -f Dockerfile.test \
    -t 192.168.57.27:5000/books-ms-tests \
    .

docker push 192.168.57.27:5000/books-ms-tests

docker-compose \
    -f docker-compose-dev.yml \
    run --rm tests

docker build \
    -f Dockerfile \
    -t 192.168.57.27:5000/books-ms \
    .

docker push 192.168.57.27:5000/books-ms

docker pull mongo

docker pull jenkins

yum install -y openjdk-7-jdk

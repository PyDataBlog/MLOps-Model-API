FROM ubuntu:14.04

MAINTAINER santoshsahoo

RUN apt-get update -qq
RUN apt-get dist-upgrade -yqq

RUN apt-get install haproxy -yqq
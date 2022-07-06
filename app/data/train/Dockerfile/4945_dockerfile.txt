FROM ubuntu:14.04
MAINTAINER danielc@pobox.com

RUN apt-get update && apt-get upgrade -y && apt-get update
RUN apt-get -y install software-properties-common apt-transport-https

FROM ubuntu:latest
MAINTAINER Julien Dauphant

RUN apt-get update
RUN apt-get install -y openssh-client && apt-get clean

ENTRYPOINT ["ssh"]

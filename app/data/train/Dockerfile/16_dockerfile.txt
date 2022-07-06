#
# Node.js v0.11.x Dockerfile
# https://github.com/hden/nodejs-v0.11
#

FROM dockerfile/python
MAINTAINER Haokang Den <haokang.den@gmail.com>

ENV PATH $HOME/.nodebrew/current/bin:$PATH

RUN cd /data && \
    curl -L git.io/nodebrew | perl - setup && \
    nodebrew install-binary v0.11.13 && \
    nodebrew use v0.11.13

# Base image
FROM resin/rpi-raspbian:jessie
MAINTAINER github.com/jottr

# Install dependencies
RUN apt-get update && apt-get install -y \
    python \
    python-dev \
    python-pip \
    python-virtualenv \
    --no-install-recommends \
&& apt-get clean \
&&  rm -rf /var/lib/apt/lists/*

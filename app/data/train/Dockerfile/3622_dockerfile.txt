FROM ubuntu:14.04
LABEL maintainer "Dave Larson <delarson@wustl.edu>"

# Volumes
VOLUME /build
VOLUME /release

# Build dependencies
RUN EXTRACT_SV_READS_VERSION=1.1.0 \
    && buildDeps=' \
        build-essential \
        git-core \
        cmake \
        libcurl4-openssl-dev \
        zlib1g-dev \
        curl \
        ca-certificates \
        ' \
    && apt-get update -qq \
    && apt-get -y install \
        --no-install-recommends \
        $buildDeps

WORKDIR /build

# Import makefile
COPY ./Makefile /build

CMD make

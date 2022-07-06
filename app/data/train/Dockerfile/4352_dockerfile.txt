FROM ubuntu:16.04

MAINTAINER Eran Segal <eran@sphericalcow.io>

USER root
ARG NUM_CORES=2

# Add openMVG binaries to path
#ENV PATH $PATH:/opt/openMVG_Build/install/bin

## Install packages
RUN apt-get update -q && \
    apt-get install -y --fix-missing --no-install-recommends \
    # Basics
    ca-certificates \
    wget \
    build-essential \
    cmake \
    graphviz \
    git \
    gcc-4.8 \ 
    gcc-4.8-multilib \  
    libpng-dev \
    libjpeg-dev \
    libtiff-dev \
    libxxf86vm1 \
    libxxf86vm-dev \
    libxi-dev \
    libxrandr-dev \
    python-dev \  
    python-pip

# Clone the openvMVG repo 
WORKDIR /tmp
RUN git clone --recursive --depth=1 https://github.com/openMVG/openMVG && \
    mkdir /opt/openMVG && \
    cd /opt/openMVG && \
    cmake -DCMAKE_BUILD_TYPE=RELEASE \
          -DCMAKE_INSTALL_PREFIX="/opt/openMVG" \
          -DOpenMVG_BUILD_TESTS=ON \
          -DOpenMVG_BUILD_EXAMPLES=ON \
          /tmp/openMVG/src/ && \
    make -j $NUM_CORES && \
    make test

# cleanup
RUN ldconfig && \
    apt-get autoclean && apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

WORKDIR /root
CMD /bin/bash

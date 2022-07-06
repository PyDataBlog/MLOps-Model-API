FROM centos:6

MAINTAINER Jackson Argo <jackson@jacksonargo.com>

ENV NVIDIA_VERSION=340.29
ENV CUDA_VERSION=6.5

# Install EPEL repo
RUN rpm -i http://mirror.ancl.hawaii.edu/linux/epel/6/i386/epel-release-6-8.noarch.rpm

# Install CUDA repo
RUN rpm -i http://developer.download.nvidia.com/compute/cuda/repos/rhel6/x86_64/cuda-repo-rhel6-${CUDA_VERSION}-14.x86_64.rpm

# Update packages
RUN yum -y update

# Install Nvidia Driver
RUN yum -y install xorg-x11-drv-nvidia-${NVIDIA_VERSION}


FROM quay.io/ukhomeofficedigital/centos-base

# Install unzip util
RUN yum update -y && yum install unzip -y

# Install vault client
RUN curl -L -O https://dl.bintray.com/mitchellh/vault/vault_0.2.0_linux_amd64.zip
RUN unzip -q vault_0.2.0_linux_amd64.zip -d /opt/bin
ENV PATH /opt/bin:$PATH

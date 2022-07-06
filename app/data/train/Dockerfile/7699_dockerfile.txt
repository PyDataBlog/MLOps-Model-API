FROM ubuntu:15.04

MAINTAINER c0d3r85@gmail.com

RUN apt-get update
RUN apt-get upgrade --with-new-pkgs -y
# RUN apt-get install wget -y

WORKDIR workspace

RUN apt-get install python git -y --force-yes

# RUN wget -qO- https://deb.nodesource.com/setup_4.x | bash -
ADD https://deb.nodesource.com/setup_4.x ./node_setup.sh
RUN bash node_setup.sh
RUN apt-get install --yes nodejs
RUN npm install -g cordova ionic gulp bower

EXPOSE 8100 35729

ENTRYPOINT ["/bin/bash"]


FROM docker.io/debian:stretch

RUN DEBIAN_FRONTEND=noninteractive apt-get update \
&& apt-get install -y git-core python3-pip libssl-dev libffi-dev \
&& pip3 install --no-input --user --upgrade git+https://github.com/curious-containers/cc-container-worker.git@0.12

FROM ubuntu:18.04

RUN apt-get -y update && \
    apt-get install -y git make build-essential libnl-3-dev libnl-genl-3-dev binutils-dev libreadline6-dev && \
    git clone https://github.com/pavel-odintsov/drop_watch.git && \
    cd drop_watch/src/ && \
    make

CMD ["/drop_watch/src/dropwatch", "-l", "kas"]

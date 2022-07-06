FROM node:latest
MAINTAINER Victor Bocharsky <bocharsky.bw@gmail.com>

RUN npm install -g bower
VOLUME ["/data"]
WORKDIR /data

ENTRYPOINT ["/usr/local/bin/bower", "--allow-root"]
CMD ["help"]

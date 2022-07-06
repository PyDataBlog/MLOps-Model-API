FROM picymru/base:latest
MAINTAINER Matthew Gall <matthew@picymru.org.uk>

RUN apk-install mongodb

EXPOSE 27017 28017

ENTRYPOINT ["/sbin/tini"]
CMD ["mongod", "--httpinterface", "--rest", "--master"]
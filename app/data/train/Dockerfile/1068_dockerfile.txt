FROM treibholz/tj3:latest
MAINTAINER Klaus Umbach <klaus+docker@uxix.de>

EXPOSE 8474
EXPOSE 8080
COPY taskjuggler.rc /tj3

VOLUME /tj3
WORKDIR /tj3
USER tj3

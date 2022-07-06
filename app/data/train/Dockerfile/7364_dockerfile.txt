FROM alpine:latest

LABEL maintainer="Piers Finlayson <piers@piersandkatie.com>"

LABEL description="Motion Container"

LABEL url="https://github.com/piersfinlayson/containers/tree/master/motion"

RUN echo "http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories && apk --no-cache add msmtp motion tzdata

RUN mkdir -p /data && chmod 666 /data

VOLUME ["/data"]

EXPOSE 8080
EXPOSE 8081

CMD ["/usr/bin/motion"]

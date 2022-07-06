FROM alpine:3.2
MAINTAINER Oleksii Fedorov <oleksii.fedorov@brainly.com>

RUN apk update
RUN apk add py-pip
RUN apk add git

ENV DOCKER_COMPOSE_COMMIT_SHA=19271898de2228767d93ddca18cc90b4e91da4ec
ENV DOCKER_COMPOSE_VERSION=git+https://github.com/docker/compose.git@$DOCKER_COMPOSE_COMMIT_SHA

RUN pip install $DOCKER_COMPOSE_VERSION

FROM alpine:3.3

RUN apk add --no-cache curl

ENV DOCKER_BUCKET github.com/armhf-docker-library/binaries/raw/master
ENV DOCKER_VERSION 1.9.1
ENV DOCKER_SHA256 2750deeb0994cce37f94c3d63b2a2544e006d67b243fc88498a43fc7c250a67d

RUN curl -fSL "https://${DOCKER_BUCKET}/docker-$DOCKER_VERSION" -o /usr/local/bin/docker \
	&& echo "${DOCKER_SHA256}  /usr/local/bin/docker" | sha256sum -c - \
	&& chmod +x /usr/local/bin/docker

COPY docker-entrypoint.sh /usr/local/bin/

ENTRYPOINT ["docker-entrypoint.sh"]
CMD ["sh"]

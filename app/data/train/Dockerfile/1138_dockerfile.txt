FROM alpine:3.4
MAINTAINER info@signalsciences.com

# groff, less is needed since some help pages "aws s3 help" require it -- why??

RUN set -ex \
    && apk add --update python python-dev py-pip gcc libc-dev groff less \
    && pip install awscli \
    && apk del python-dev gcc libc-dev
ENTRYPOINT [ "/usr/bin/aws" ]

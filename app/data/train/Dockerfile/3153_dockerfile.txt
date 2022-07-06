FROM python:3.5.1

MAINTAINER Soloman Weng "soloman1124@gmail.com"
ENV REFRESHED_AT 2016-05-16

RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app


ADD ./requirements /usr/src/app/requirements
RUN pip install -r requirements/development.txt

ADD . /usr/src/app

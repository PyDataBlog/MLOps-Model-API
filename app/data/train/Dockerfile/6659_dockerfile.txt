FROM nginx:1.9
MAINTAINER Duane Leem <duaneleem@alumni.ggu.edu>

## Install tools.
RUN apt-get update && apt-get install -y \
    nano

## NGINX & PHP configurations
COPY "./artifacts/nginx/nginx.conf" "/etc/nginx/nginx.conf"

## Required to use nano.
ENV TERM xterm

## Port configurations.
EXPOSE 80 443

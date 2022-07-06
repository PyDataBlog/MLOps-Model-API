FROM ubuntu:14.04

MAINTAINER Martin Westergaard Lassen <mwl@trifork.com>

ADD http://packages.erlang-solutions.com/site/esl/mongooseim/FLAVOUR_1_main/mongooseim_1.4.0-1~ubuntu~trusty_amd64.deb /tmp/mongooseim.deb

RUN apt-get update && \
    apt-get install -y \
        libodbc1 \
        unixodbc && \
    dpkg -i /tmp/mongooseim.deb && \
    rm -rf /var/lib/apt/lists/* /tmp/mongooseim.deb

EXPOSE 5222 5280 5269

CMD /usr/lib/mongooseim/bin/mongooseim console -noshell

FROM ubuntu:trusty

MAINTAINER Martin Westergaard Lassen <mwl@trifork.com>

ADD https://packages.erlang-solutions.com/erlang/mongooseim/FLAVOUR_1_main/mongooseim_1.5.1-2~ubuntu~trusty_amd64.deb /tmp/mongooseim.deb

RUN apt-get update && \
    apt-get install -y \
        libodbc1 \
        unixodbc && \
    dpkg -i /tmp/mongooseim.deb && \
    rm -rf /var/lib/apt/lists/* /tmp/mongooseim.deb

EXPOSE 5222 5280 5269

CMD /usr/lib/mongooseim/bin/mongooseim console -noshell

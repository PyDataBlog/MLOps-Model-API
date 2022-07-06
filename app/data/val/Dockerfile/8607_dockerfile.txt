FROM ubuntu:12.04
EXPOSE 5984

RUN echo "deb http://us.archive.ubuntu.com/ubuntu/ precise main restricted \n\
deb-src http://us.archive.ubuntu.com/ubuntu/ precise main restricted \n\
deb http://us.archive.ubuntu.com/ubuntu/ precise-updates main restricted \n\
deb-src http://us.archive.ubuntu.com/ubuntu/ precise-updates main restricted \n\
deb http://us.archive.ubuntu.com/ubuntu/ precise universe \n\
deb-src http://us.archive.ubuntu.com/ubuntu/ precise universe \n\
deb http://us.archive.ubuntu.com/ubuntu/ precise-updates universe \n\
deb-src http://us.archive.ubuntu.com/ubuntu/ precise-updates universe \n\
deb http://us.archive.ubuntu.com/ubuntu/ precise multiverse \n\
deb-src http://us.archive.ubuntu.com/ubuntu/ precise multiverse \n\
deb http://us.archive.ubuntu.com/ubuntu/ precise-updates multiverse \n\
deb-src http://us.archive.ubuntu.com/ubuntu/ precise-updates multiverse \n\
deb http://us.archive.ubuntu.com/ubuntu/ precise-backports main restricted universe multiverse \n\
deb-src http://us.archive.ubuntu.com/ubuntu/ precise-backports main restricted universe multiverse \n\
deb http://security.ubuntu.com/ubuntu precise-security main restricted \n\
deb-src http://security.ubuntu.com/ubuntu precise-security main restricted \n\
deb http://security.ubuntu.com/ubuntu precise-security universe \n\
deb-src http://security.ubuntu.com/ubuntu precise-security universe \n\
deb http://security.ubuntu.com/ubuntu precise-security multiverse \n\
deb-src http://security.ubuntu.com/ubuntu precise-security multiverse" > /etc/apt/sources.list
RUN apt-get update
RUN apt-get install -y g++ erlang-dev erlang-manpages erlang-base-hipe erlang-eunit erlang-nox erlang-xmerl erlang-inets libmozjs185-dev libicu-dev libcurl4-gnutls-dev libtool wget make python-simplejson
ADD http://www.eu.apache.org/dist/couchdb/source/1.5.1/apache-couchdb-1.5.1.tar.gz /tmp/apache-couchdb-1.5.1.tar.gz
RUN cd /tmp && tar xvzf apache-couchdb-1.5.1.tar.gz
RUN cd /tmp/apache-couchdb-* && ./configure && make && make install
RUN ln -s /usr/local/etc/logrotate.d/couchdb /etc/logrotate.d/couchdb
RUN ln -s /usr/local/etc/init.d/couchdb /etc/init.d
RUN update-rc.d couchdb defaults

RUN echo "[httpd]\n\
bind_address = 0.0.0.0\n\
port = 5984" > /usr/local/etc/couchdb/local.ini

ENV ERL_MAX_PORTS 4096
ENV ERL_FLAGS "+A 4"

CMD ["/usr/local/bin/couchdb"]

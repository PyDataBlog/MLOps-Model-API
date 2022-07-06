
FROM ubuntu:14.04

MAINTAINER Joacim Turesson, joacim.turesson@autenta.se

RUN locale-gen sv_SE.UTF-8
RUN update-locale LANG=sv_SE.UTF-8

RUN apt-get -qq update && DEBIAN_FRONTEND=noninteractive apt-get install -y -q python-software-properties software-properties-common postgresql-common postgresql-9.3 postgresql-contrib-9.3 postgresql-9.3-postgis-2.1 libpq-dev sudo

RUN mkdir /etc/ssl/private-copy; mv /etc/ssl/private/* /etc/ssl/private-copy/; rm -r /etc/ssl/private; mv /etc/ssl/private-copy /etc/ssl/private; chmod -R 0700 /etc/ssl/private; chown -R postgres /etc/ssl/private

ADD src/main/conf/postgresql.conf /etc/postgresql/9.3/main/postgresql.conf
ADD src/main/conf/pg_hba.conf /etc/postgresql/9.3/main/pg_hba.conf
RUN chown postgres:postgres /etc/postgresql/9.3/main/*.conf
ADD src/main/script/run.sh /usr/local/bin/run
RUN chmod +x /usr/local/bin/run

VOLUME ["/var/lib/postgresql"]

EXPOSE 5432

CMD ["/usr/local/bin/run"]

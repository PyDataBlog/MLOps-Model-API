FROM mysql

ADD data/export.sql /docker-entrypoint-initdb.d/export.sql


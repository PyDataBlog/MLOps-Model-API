FROM mysql
COPY ./my.cnf /etc/mysql/conf.d/
ENV MYSQL_DATABASE=sampledb \
    MYSQL_USER=sampleuser \
    MYSQL_PASSWORD=secret \
    MYSQL_ROOT_PASSWORD=secret

EXPOSE 3306:3306
CMD ["mysqld"]

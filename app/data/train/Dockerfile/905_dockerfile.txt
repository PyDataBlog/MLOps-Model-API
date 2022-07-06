FROM ubuntu:14.04
MAINTAINER Jason Gerfen <jason.gerfen@utah.edu>

# Update the system
RUN apt-get update && apt-get upgrade -y

# Add common shell scripts
ADD common/create-accounts /usr/bin/
ADD common/openssh-service /usr/bin/

# Create some user accounts
RUN /usr/bin/create-accounts

# Ensure we can manage the container
RUN /usr/bin/openssh-service
RUN service ssh start

# Copy over any keys
ADD common/keys/ /etc/ssh/keyring

# Set some env vars
ENV MYSQL_USER admin
ENV MYSQL_PASS password
ENV TERM dumb

# Copy over mariadb installer
ADD common/install-mariadb /usr/bin/
RUN /usr/bin/install-mariadb

RUN service mysql stop
RUN sed -i -e"s/^bind-address\s*=\s*127.0.0.1/bind-address = 0.0.0.0/" /etc/mysql/my.cnf

RUN /usr/sbin/mysqld & \
        sleep 10s && \
        echo "GRANT ALL ON *.* TO admin@'%' IDENTIFIED BY 'password' WITH GRANT OPTION; FLUSH PRIVILEGES" | mysql
CMD ["/usr/bin/mysqld_safe", "&"]

# Ensure mariadb & ssh is accessible
EXPOSE 22 3306

# Post configuration script 
ADD common/custom-commands /usr/bin/
RUN /usr/bin/custom-commands

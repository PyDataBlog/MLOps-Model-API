FROM ubuntu:latest

ENV INFLUXDB_VERSION 0.8.8

# Prevent some error messages
ENV DEBIAN_FRONTEND noninteractive

RUN		apt-get -y update && apt-get -y upgrade && apt-get -y install wget

# Install InfluxDB
RUN		wget http://s3.amazonaws.com/influxdb/influxdb_${INFLUXDB_VERSION}_amd64.deb && \
			dpkg -i influxdb_${INFLUXDB_VERSION}_amd64.deb && rm influxdb_${INFLUXDB_VERSION}_amd64.deb
 

EXPOSE	8083 8086 8084

ADD ./start.sh /usr/local/bin/start.sh
RUN chmod +x /usr/local/bin/start.sh

CMD ["/usr/local/bin/start.sh"]

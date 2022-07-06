FROM dockerfile/java:openjdk-7-jre
MAINTAINER Jerome Terry, jerome.l.terry@gmail.com

RUN wget -O - http://debian.neo4j.org/neotechnology.gpg.key| apt-key add -  && \
	echo 'deb http://debian.neo4j.org/repo stable/' > /etc/apt/sources.list.d/neo4j.list && \
	apt-get update && apt-get install -y \
	neo4j

ADD launch.sh /
RUN chmod +x /launch.sh
VOLUME /var/lib/neo4j/data
EXPOSE 7474 1337
WORKDIR /
CMD ["/bin/bash", "-c", "/launch.sh"]
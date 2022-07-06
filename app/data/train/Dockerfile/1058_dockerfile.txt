FROM logicify/java8

ENV TOMCAT_VERSION 8.0.23

USER app

RUN cd /srv && \
  (curl -L http://mirrors.ibiblio.org/apache/tomcat/tomcat-8/v$TOMCAT_VERSION/bin/apache-tomcat-$TOMCAT_VERSION.tar.gz | gunzip -c | tar x) \
  && ln -s /srv/apache-tomcat-$TOMCAT_VERSION /srv/apache-tomcat \
  && rm -fR /srv/apache-tomcat/webapps/*
 
ADD context.xml /srv/apache-tomcat/conf/
ADD server.xml /srv/apache-tomcat/conf/

EXPOSE 8080
CMD ["/srv/apache-tomcat/bin/catalina.sh", "run"]

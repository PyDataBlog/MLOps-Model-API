FROM openjdk
EXPOSE 8080
ADD ./dist/app.jar /srv/app.jar
ADD ./dist/apache-logging-log4j.jar /srv/apache-logging-log4j.jar
ADD ./dist/bson-3.0.2.jar /srv/bson-3.0.2.jar
ADD ./dist/json.jar /srv/json.jar
ADD ./dist/json-simple-1.1.jar /srv/json-simple-1.1.jar
ADD ./dist/mongodb-driver-3.3.0.jar /srv/mongodb-driver-3.3.0.jar
ADD ./dist/mongodb-driver-async-3.3.0.jar /srv/mongodb-driver-async-3.3.0.jar
ADD ./dist/mongodb-driver-core-3.3.0.jar /srv/mongodb-driver-core-3.3.0.jar
ADD ./dist/mongo-java-driver-3.3.0.jar /srv/mongo-java-driver-3.3.0.jar
ADD ./dist/org.json.jar /srv/org.json.jar
ADD ./dist/org.restlet.ext.fileupload_1.2.jar /srv/org.restlet.ext.fileupload_1.2.jar
ADD ./dist/org.restlet.ext.json.jar /srv/org.restlet.ext.json.jar
ADD ./dist/org.restlet.ext.servlet.jar /srv/org.restlet.ext.servlet.jar
ADD ./dist/org.restlet.ext.xml.jar /srv/org.restlet.ext.xml.jar
ADD ./dist/org.restlet.jar /srv/org.restlet.jar
ADD ./dist/restlet.jar /srv/restlet.jar
ADD ./dist/restlet-json.jar /srv/restlet-json.jar
ADD ./dist/servlet-api.jar /srv/servlet-api.jar
ADD ./dist/app.jar /srv/app.jar

CMD java -cp /srv/servlet-api.jar:/srv/restlet-json.jar:/srv/restlet.jar:/srv/org.restlet.jar:/srv/org.restlet.ext.xml.jar:/srv/org.restlet.ext.servlet.jar:/srv/org.restlet.ext.json.jar:/srv/org.restlet.ext.fileupload_1.2.jar:/srv/org.json.jar:/srv/mongo-java-driver-3.3.0.jar:/srv/mongodb-driver-core-3.3.0.jar:/srv/mongodb-driver-async-3.3.0.jar:/srv/mongodb-driver-3.3.0.jar:/srv/json-simple-1.1.jar:/srv/json.jar:/srv/apache-logging-log4j.jar:/srv/bson-3.0.2.jar:/srv/app.jar api.ServerApplication
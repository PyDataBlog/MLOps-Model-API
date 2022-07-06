FROM openjdk:8

ARG version
ARG jar
#ARG war


LABEL description="SwampUP 2017 Mere Mortals Docker service example."
LABEL version=$version

EXPOSE 8080

# says jar, but we mean war.
COPY build/libs/$jar /app/app.war
COPY build/jetty-runner/jetty-runner-*.jar /app/jetty-runner.jar

ENTRYPOINT [ "/usr/bin/java", "-jar", "/app/jetty-runner.jar", "/app/app.war" ]

FROM java:8
ADD target/speedlaw-0.1.0-SNAPSHOT.jar /srv/speedlaw-app.jar
EXPOSE 8080
CMD ["java", "-jar", "/srv/myproj-app.jar"]


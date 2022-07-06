FROM openjdk:8
MAINTAINER bill@billbensing.com
EXPOSE 3000
ADD /target/wb3-configserver.jar wb3-configserver.jar
ENTRYPOINT ["java", "-jar", "wb3-configserver.jar"]
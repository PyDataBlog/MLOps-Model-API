FROM java:7

MAINTAINER xausky xausky@gmail.com

ENV KAFKA_VERSION 0.10.0.1
ENV SCALA_VERSION 2.10

WORKDIR /

COPY initialize.sh .

RUN ./initialize.sh

WORKDIR /kafka

COPY entrypoint.sh .

ENTRYPOINT ["./entrypoint.sh"]

EXPOSE 2181 9092

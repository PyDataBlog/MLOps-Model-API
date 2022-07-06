# our base image
FROM ubuntu:12.04

RUN apt-get -y update && apt-get -y install mono-complete

ADD dotNet/server.exe /usr/src/app/ 

EXPOSE 9090

CMD ["mono", "/usr/src/app/server.exe"]


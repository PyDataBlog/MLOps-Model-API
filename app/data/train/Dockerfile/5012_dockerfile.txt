FROM ubuntu:trusty
RUN apt-get update
RUN apt-get install -y ngircd
RUN sed -i -r "s/.*Listen = 127.0.0.1.*/\tListen = 0.0.0.0/" /etc/ngircd/ngircd.conf
EXPOSE 6667
CMD ["ngircd", "--nodaemon"]

FROM pataquets/ubuntu:trusty

RUN \
  apt-get update && \
    DEBIAN_FRONTEND=noninteractive \
    apt-get -y install \
      php5-cli \
      php5-mongo \
  && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

ADD https://github.com/bobthecow/genghis/raw/master/genghis.php /root/

CMD [ "php", "--server", "0.0.0.0:80", "--docroot", "/root", "/root/genghis.php" ]

EXPOSE 80

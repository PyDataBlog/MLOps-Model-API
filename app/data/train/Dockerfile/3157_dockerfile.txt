FROM uchiwa/uchiwa
MAINTAINER Christian Sakshaug <christian@csadevio.net>

RUN wget https://github.com/jwilder/dockerize/releases/download/v0.0.2/dockerize-linux-amd64-v0.0.2.tar.gz
RUN tar -C /usr/local/bin -xzvf dockerize-linux-amd64-v0.0.2.tar.gz

ADD config.json.tmpl /config/config.json.tmpl
ADD start /start-uchiwa

RUN chmod 755 /start-uchiwa

ENTRYPOINT ["/start-uchiwa"]

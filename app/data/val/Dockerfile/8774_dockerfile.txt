# httpd container for setting up Apache mod_fcgi_proxy to application containers

FROM httpd:2.4
MAINTAINER Jeff Braucher <jeff@braucher.net>

# Install some tools
RUN apt-get update && apt-get install -y vim less

# Setup apache
RUN echo "Include conf/fcgi.conf" >> /usr/local/apache2/conf/httpd.conf
COPY ./fcgi.conf /usr/local/apache2/conf/

COPY ./app* /
ENTRYPOINT ["/app"]
CMD ["start"]


FROM ubuntu:18.10

LABEL maintainer="jmcoimbra.eng.br"

# Avoid apps to ask questions during instalation
ENV DEBIAN_FRONTEND=noninteractive

### Installing Apache httpd
RUN apt-get update; \
	apt-get install -y apache2;

# Configuring httpd
COPY ./000-default.conf /etc/apache2/sites-enabled/000-default.conf
COPY ./apache2.conf /etc/apache2/apache2.conf
COPY ./ssl /etc/apache2/ssl
COPY ./default-ssl.conf /etc/apache2/sites-available/default-ssl.conf

RUN a2enmod rewrite; \
  a2enmod vhost_alias; \
	a2enmod ssl; \
	gpasswd -a root www-data; \
	chown -R www-data:www-data /var/www; \
	a2ensite default-ssl.conf;

# Timezone information for "tzdata"
ENV TZ=America/Sao_Paulo
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# Including PHP
RUN apt-get update; \
	apt-get install -y php libapache2-mod-php php-mysql php-bcmath php-curl php-imagick php-gd;

# Configuring PHP through php.ini
COPY ./php.ini /etc/php/7.2/apache2/php.ini

####### DEV STUFF #######
RUN apt-get install curl vim lsof -y

# Finalizing...
EXPOSE 80 443

CMD ["apachectl", "-D", "FOREGROUND"]

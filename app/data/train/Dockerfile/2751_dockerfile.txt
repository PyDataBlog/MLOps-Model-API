FROM php:5.6-apache

MAINTAINER Jason Callaway <jcallawa@redhat.com>

COPY index.php /var/www/html/
RUN rm -f /var/www/html/index.html

EXPOSE 80

CMD ["sh", "-c", "apache2-foreground"]

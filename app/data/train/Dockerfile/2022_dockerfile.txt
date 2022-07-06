# Esto es un comentario.

# Utilizamos una imagen existente. Estas se encuentran en https://hub.docker.com/_/wordpress/
# Esta imagen requiere parámetros especificos para funcionar, por lo que leer la documentación es importante.
FROM wordpress:latest

# MAINTAINER indica quien es el autor del archivo docker.
MAINTAINER Alejandro Madrigal Leiva

# Variables de ambiente (los parámetros antes mencionados que previamente se usaban como parte del comando).
ENV WORDPRESS_DB_USER=root
ENV WORDPRESS_DB_PASSWORD=root
ENV WORDPRESS_DB_HOST=wordcamp_cr_2016_db
ENV WORDPRESS_DB_NAME=wordcampcr2016

# Esta parte require un poco de experiencia instalando librerías desde línea de comandos. Es solo cuestión de práctica.

# Instala librerías del sistema.
RUN \
    apt-get update -y && \
    apt-get install unzip

# Instala WP-CLI.
RUN \
    cd /tmp && \
    curl -OL https://raw.githubusercontent.com/wp-cli/builds/gh-pages/phar/wp-cli.phar && \
    chmod +x wp-cli.phar && \
    mv wp-cli.phar /usr/local/bin/wp

# Copia archivos de configuración.
COPY configuracion/php/wordcampcr_2016.ini /usr/local/etc/php/conf.d/wordcampcr_2016.ini
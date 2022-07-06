FROM debian:wheezy

MAINTAINER zoxi

# Fichiers additionels
######################

# Ajout des fichiers BASH
ADD install.sh /install.sh

# Installation
##############

# Execution du BASH (mises à jour, locales, user nobody et clean)
RUN chmod +x /install.sh && \
            /bin/bash /install.sh

#Environement
#############

# Let the container know that there is no tty
ENV DEBIAN_FRONTEND noninteractive

#Language
ENV LANGUAGE en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8

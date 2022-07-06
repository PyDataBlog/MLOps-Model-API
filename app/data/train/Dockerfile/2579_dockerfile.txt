# Use phusion/baseimage as base image. To make your builds reproducible, make
# sure you lock down to a specific version, not to `latest`!
#  See https://github.com/phusion/baseimage-docker/blob/master/Changelog.md for
# a list of version numbers.
FROM phusion/baseimage:0.9.9
MAINTAINER MINETTE Alexandre, minette.alexandre@gmail.com
VOLUME   ["/var/downloads"]

# Set correct environment variables.
ENV HOME /root

# Regenerate SSH host keys. baseimage-docker does not contain any, so you
# have to do that yourself. You may also comment out this instruction; the
# init system will auto-generate one during boot.
RUN /etc/my_init.d/00_regen_ssh_host_keys.sh

# Use baseimage-docker's init system.
CMD ["/sbin/my_init"]

RUN echo 'ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDrCt4nVBbO3toUJmpwdkJnAURNieMl3nVCcRu7vF3B590xRuJyBenIrUFba4XzgHtDuf+k+KCfLG46FMl6BpoNW+uixeMn50Hb0GSeBd7WxVwYbpPfR7qVYL5qYm3p2kdobiCRL8QEoSzt3EL06lYYopUDhJBphla2LgCHvslnNGdQPx7K8WlfyQkcxUC4MLBTwVYPOwfzNOfKCZHtbeWq2RdIefgRkXKsigNltzhbFN/6g+98sOqux2dgIJp9hdDO6Ej4u1VF1JrNF9jgxc5mv8/tDXT7hjXrJUoTZzxpivOzvh2JHExBkrm9zVR4Cx4L/OWy0F95OgQoMeMXY0aZ perso@pelos-K53SC.home' > /root/.ssh/authorized_keys


# installing transmission
RUN apt-get update
RUN apt-get install -y transmission-cli transmission-daemon

# setup the deamon...
RUN curl https://raw.githubusercontent.com/alex-min/raspbmc-docker-bundles/master/transmission/settings.json > /etc/transmission-daemon/settings_override.json
RUN mkdir -p /etc/service/transmission-daemon
RUN curl https://raw.githubusercontent.com/alex-min/raspbmc-docker-bundles/master/transmission/transmission-daemon.sh > /etc/service/transmission-daemon/run
RUN chmod +x /etc/service/transmission-daemon/run


RUN chown debian-transmission:debian-transmission -R /var/downloads
RUN echo 'Files will be downloaded here' > /var/downloads/README.txt
# start the deamon 

# Clean up APT when done.
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Web Interface
EXPOSE 9091
# ssh
EXPOSE 22
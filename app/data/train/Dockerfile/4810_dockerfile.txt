FROM piotrminkina/debian:lenny
MAINTAINER Piotr Minkina <projects[i.am.spammer]@piotrminkina.pl>


ADD bin/policy-rc.d /usr/sbin/policy-rc.d
ADD bin/start-stop-daemon /sbin/start-stop-daemon

RUN apt-get update \
 && DEBIAN_FRONTEND=noninteractive apt-get install -yqq \
    busybox \
    apache2-mpm-worker \
    libapache2-mod-fastcgi \
    libapache2-mod-geoip \
 && find /var/cache/apt/archives/ /var/lib/apt/lists/ -type f -exec rm {} +

ADD envvars /etc/apache2/envvars
ADD conf.d/ /etc/apache2/conf.d/
COPY entrypoint.sh /usr/local/bin/entrypoint

EXPOSE 80 443
ENTRYPOINT ["/usr/local/bin/entrypoint"]
CMD []

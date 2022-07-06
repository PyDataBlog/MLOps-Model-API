FROM dockerfile/ubuntu
MAINTAINER Ross Zurowski <ross@rosszurowski.com>

# Don't give us warnings about ttys
ENV DEBIAN_FRONTEND noninteractive

# Get core utils
RUN apt-get update -y -qq && \
 	apt-get install -y -qq git wget curl software-properties-common && \
 	apt-get -y clean

# Install Nginx
RUN add-apt-repository -y ppa:nginx/stable && \
	apt-get update -y -qq && \
	apt-get -y -qq install nginx && \
	apt-get -y clean

# Update Nginx configs
RUN echo "daemon off;" >> /etc/nginx/nginx.conf

# Install HHVM
RUN apt-key adv --recv-keys --keyserver hkp://keyserver.ubuntu.com:80 0x5a16e7281be7a449 2>&1 && \
 	add-apt-repository 'deb http://dl.hhvm.com/ubuntu trusty main' 2>&1 && \
 	apt-get update -qq && \
 	apt-get install -y -qq hhvm && \
 	apt-get -y -qq clean

# Update HHVM configs
RUN echo "date.timezone = America/Toronto" >> /etc/hhvm/php.ini && \
	/usr/share/hhvm/install_fastcgi.sh && \
	update-rc.d hhvm defaults && \
	service hhvm restart
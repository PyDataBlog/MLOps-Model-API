#!/bin/bash

NGINX="/etc/nginx/sites-enabled/"


if [ "$EUID" -ne 0 ]; then
	echo ""
	echo "Please run this script as root."
	echo ""
	exit;
fi

if [ ! -d /var/www/certbot ]; then
	sudo mkdir -p /var/www/certbot
fi

# Setup Domain
echo "Enter your root Domain Name: (DNS Should already propogate to this server)"
read DOMAIN;
echo "Setting up $DOMAIN"
echo ""

# Does the domain exist?
if [ -d /var/www/$DOMAIN ]; then
	echo ""
	echo "This domain already exists in the www folder."
	echo ""
	exit
fi


# Make web root
echo "Making web root"
sudo mkdir /var/www/$DOMAIN
sudo mkdir /var/www/$DOMAIN/www

# Set CONF Path
FILE="$NGINX$DOMAIN.conf"


# Create base file
echo "Creating config file"
sudo touch $FILE
sudo /bin/cat <<EOM > $FILE
# Site: $DOMAIN
server {
	listen 80;
	server_name $DOMAIN www.$DOMAIN;

	location '/.well-known/acme-challenge' {
		root /var/www/certbot;
	}
}
EOM

# reload nginx
echo "Reloading NGINX"
sudo nginx -t && sudo nginx -s reload


# install cert
echo "Installering SSL Certificate"
sudo certbot certonly -a webroot --webroot-path=/var/www/certbot -d $DOMAIN -d www.$DOMAIN

# rewrite hardened nginx conf file with cert paths
echo "Updating config file fore certiticate"
sudo /bin/cat <<EOM > $FILE
# Site: $DOMAIN
server {
	listen 80;
	listen 443 ssl http2;
	server_name $DOMAIN www.$DOMAIN;
	ssl_protocols TLSv1.2;
	ssl_ciphers 'ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:EC$
	ssl_prefer_server_ciphers On;
	ssl_certificate /etc/letsencrypt/live/$DOMAIN/fullchain.pem;
	ssl_certificate_key /etc/letsencrypt/live/$DOMAIN/privkey.pem;
	ssl_trusted_certificate /etc/letsencrypt/live/$DOMAIN/chain.pem;
	ssl_session_cache shared:SSL:128m;
	add_header Strict-Transport-Security "max-age=31557600; includeSubDomains";
	add_header X-Frame-Options "SAMEORIGIN" always;
	add_header X-Content-Type-Options "nosniff" always;
	add_header X-Xss-Protection "1";
	add_header Content-Security-Policy "default-src 'self'; script-src 'self' *.google-analytics.com";
	add_header Referrer-Policy origin-when-cross-origin;
	ssl_stapling on;
	ssl_stapling_verify on;
	resolver 8.8.8.8;

	root /var/www/$DOMAIN/www;
	index index.html;

	location '/.well-known/acme-challenge' {
		root /var/www/certbot;
	}

	location / {
		if ($scheme = http) {
			return 301 https://$server_name$request_uri;
		}
	}
}

EOM

# reload nginx
echo "Reloading NGINX after cert install"
sudo nginx -t && sudo nginx -s reload


echo ""
echo ""
echo "https://www.$DOMAIN is ready to test: "
echo "Varify at: https://securityheaders.io/?q=https://www.$DOMAIN"
echo ""
echo ""


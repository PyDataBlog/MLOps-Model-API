#!/bin/bash
# Script Name: Pyload Auto Installer
# Author: STX2k (2016)
#############################################

# Color definition variables
YELLOW='\e[33;3m'
RED='\e[91m'
BLACK='\033[0m'
CYAN='\e[96m'
GREEN='\e[92m'

# Make sure to clear the Terminal
clear

# Display the Title Information
echo -e $CYAN
echo -e ">>> Pyload - Auto Installer by STX2k <<<"
echo
echo -e ">> Check ReadME for more Infos <<"
echo
echo -e $RED' You use this Script on your own risk!'$BLACK
echo
# Function to check if running user is root
function CHECK_ROOT {
	if [ "$(id -u)" != "0" ]; then
		echo
		echo -e $RED "This script must be run as root." 1>&2
		echo
		exit 1
	fi
}
#User for Pyload
echo -e $YELLOW
echo -e "---> [For safety reasons, we create a separate user...]"$BLACK
read -p "User Account Name (eg.'pyload'):" usernamepyload
useradd -ms /bin/bash $usernamepyload
mkdir /home/$usernamepyload/downloads
echo -e $GREEN
echo -e "DONE!"
# Updating System
echo -e $YELLOW
echo -e "---> [Updating System... this can take some time!]"$BLACK
apt-get update > /dev/null
apt-get -y upgrade > /dev/null
apt-get -y dist-upgrade > /dev/null
echo -e $GREEN
echo -e "DONE!"
# Installing Basic Software
echo -e $YELLOW
<<<<<<< HEAD
echo -e "---> [Installing Basic Software... this can take some time!]"$BLACK
=======
echo -e "---> [Installing Basic Software...]"$BLACK
>>>>>>> origin/master
apt-get install -y nano curl git htop man sudo software-properties-common unzip unrar p7zip-full mediainfo wget sqlite3 python python-flask python-dev python-crypto python-pip python-imaging python-setuptools python-openssl python-pycurl gocr python-django openssl tesseract-ocr git-core > /dev/null
echo -e $GREEN
echo -e "DONE!"
# Installing Python 3 and some modules
echo -e $YELLOW
echo -e "---> [Installing Python and Modules... this can take some time!]"$BLACK
pip install Beaker MultipartPostHandler bottle colorama jinja2 markupsafe thrift BeautifulSoup colorlog feedparser pyOpenSSL pycrypto simplejson tesseract Send2Trash rhino supervisor > /dev/null
echo -e $GREEN
echo -e "DONE!"

# Install nginx
echo -e $YELLOW
echo -e "---> [Installing nginx...]"$BLACK
apt-get install -y nginx > /dev/null
echo "server {
	listen 80;
    server_name servername2;
	return 301 https://$server_name$request_uri;
}
server {
        listen      443 default_server ssl spdy;
        server_name servername2;
        root /usr/share/nginx/www;
        index index.html index.htm;
        ssl on;
        ssl_certificate /etc/letsencrypt/live/ssldomain/fullchain.pem;
        ssl_certificate_key /etc/letsencrypt/live/ssldomain/privkey.pem;
        ssl_protocols TLSv1.2 TLSv1.1 TLSv1;
        ssl_prefer_server_ciphers on;
        ssl_ciphers EECDH+ECDSA+AESGCM:EECDH+aRSA+AESGCM:EECDH+ECDSA+SHA512:EECDH+ECDSA+SHA384:EECDH+ECDSA+SHA256:ECDH+AESGCM:ECDH+AES256:DH+AESGCM:DH+AES256:RSA+AESGCM:!aNULL:!eNULL:!LOW:!RC4:!3DES:!MD5:!EXP:!PSK:!SRP:!DSS;
        ssl_session_cache shared:TLS:2m;
        # OCSP stapling
        ssl_stapling on;
        ssl_stapling_verify on;
        resolver 8.8.8.8;
        # Set HSTS to 365 days
        add_header Strict-Transport-Security 'max-age=31536000; includeSubDomains';
        location /{
                proxy_pass      http://127.0.0.1:8003/;
                include /etc/nginx/proxy_params;
        }
}" > pyload.conf
mv pyload.conf /etc/nginx/sites-enabled/pyload
echo -e $CYAN
read -p "Set Servername (eg. pyload.yourdomain.com):" servername
sed -i "s/\bservername2\b/$servername/g" /etc/nginx/sites-enabled/pyload
echo -e $CYAN
echo -e "---> [Restart nginx...]"$BLACK
systemctl restart nginx.service
echo -e $GREEN
echo -e "DONE!"
# Install Letsencrypt
echo -e $YELLOW
echo -e "---> [Installing Letsencrypt...]"$BLACK
echo -e $YELLOW
echo -e "-------------------------------------------------"
echo -e "# Please enter a domain without www!            #"
echo -e "#                                               #"
echo -e "# eg. pyload.yourdomain.com                     #"
echo -e "# you need an email Adress for the next Step    #"
echo -e "-------------------------------------------------"$BLACK
read -p "Enter a legit email Addy for SSL Cert (eg. ssl@yourdomain.com):" sslmail
systemctl stop nginx.service
cd /opt/
wget https://dl.eff.org/certbot-auto
chmod a+x ./certbot-auto
./certbot-auto certonly --standalone --email $sslmail -d $servername
echo -e $GREEN
echo -e "DONE!"

# Add SSL to Config
echo -e $YELLOW
echo -e "---> [Add SSL to the nginx config...]"$BLACK
sed -i "s/\bssldomain\b/$servername/g" /etc/nginx/sites-enabled/pyload
<<<<<<< HEAD
systemctl start nginx.service
systemctl restart nginx.service
=======
service nginx start
>>>>>>> origin/master
echo -e $GREEN
echo -e "DONE!"

# Fixing Permissions
echo -e $YELLOW
echo -e "---> [Pyload install...]"$BLACK
cd /opt/
git clone -b stable https://github.com/pyload/pyload.git
cd /opt/pyload/
echo -e $GREEN
echo -e "DONE!"
# Complete the Web Setup!
echo -e $RED"STOP! READ! STOP! READ! STOP! READ!"$BLACK
echo -e $RED"STOP! READ! STOP! READ! STOP! READ!"$BLACK
echo -e $GREEN
echo -e "-------------------------------------------------"
echo -e "# Setup is complete you must start Pyload       #"
echo -e "#                                               #"
echo -e "# I will login you, with your user for Pyload   #"
echo -e "# you must start pyload first time to complete  #"
echo -e "# the setup Wizzard..... type: ./pyLoadCore.py  #"
echo -e "#                                               #"
echo -e "# Use as Download Path your Userhome:           #"
echo -e "#                                               #"
echo -e $RED"# /home/$usernamepyload/Download                #"$GREEN
echo -e $RED"# !Set Webinterface Port: 8003                  #"$GREEN
echo -e "#                                               #"
echo -e "#  After you end the Wizzard use                #"
echo -e "#                                               #"
echo -e $RED"#  ./pyLoadCore.py --daemon                     #"$GREEN
echo -e "#                                               #"
echo -e "#  to start pyLoad as Daemon.                   #"
echo -e "#                                               #"
echo -e "#  Thank you for using my Script!               #"
echo -e "-------------------------------------------------"$BLACK
echo -e $RED"STOP! READ! STOP! READ! STOP! READ!"$BLACK
echo -e $RED"STOP! READ! STOP! READ! STOP! READ!"$BLACK
<<<<<<< HEAD
=======
read -p "Press [Enter] to start the pyLoad Setup Wizzard"
cd /opt/pyload/
su $usernamepyload
./pyLoadCore.py
>>>>>>> origin/master
echo -e "---> WE ARE DONE!"$BLACK
read -p "Enter ./pyLoadCore.py to start the pyLoad Setup Wizzard"
cd /opt/pyload/
su $usernamepyload

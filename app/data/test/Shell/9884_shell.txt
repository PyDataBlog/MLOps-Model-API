#!/bin/bash

cd ~/Documents/git/simp_le/ || exit 2

# setup virtualenv
virtualenv --no-site-packages venv
export PATH=./venv/bin:$PATH
PATH=$PWD/venv/bin:$PATH
echo "PATH=$PATH"

# install packages
for pkg in setuptools pip wheel
do
  pip install -U "${pkg?}"
done
pip install -e .

# try to renew
cd /etc/httpd/certs/sepi-dev
~/Documents/git/simp_le/simp_le.py --email chicks.net@gmail.com -f account_key.json -f fullchain.pem -f key.pem -f cert.pem -d dev.sepi.fini.net:/www/sepi-dev/docroot/ || exit 1

# bounce apache
echo "bouncing apache..."
sudo service httpd reload

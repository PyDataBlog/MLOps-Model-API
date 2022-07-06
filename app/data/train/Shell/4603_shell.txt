#!/bin/bash
sudo apt-get update
sudo apt-get install -y git curl
sudo curl -L git.io/weave -o /usr/local/bin/weave
git pull origin master
sudo chmod +x /usr/local/bin/weave
curl -fsSL https://get.docker.com/ | sh
sudo usermod -aG docker $USER

echo '***** You must now reboot. Then run ./start.sh ******'

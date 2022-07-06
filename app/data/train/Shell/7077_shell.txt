#!/bin/bash

cd ~

echo install updates
sudo apt-get update
sudo apt-get install openjdk-7-jre-headless -y
sudo apt-get install git -y

echo Installing Elastic Search
wget https://download.elasticsearch.org/elasticsearch/elasticsearch/elasticsearch-1.2.1.deb
sudo dpkg -i elasticsearch-1.2.1.deb

cd /usr/share/elasticsearch
sudo bin/plugin -install lmenezes/elasticsearch-kopf
cd -
sudo service elasticsearch start
curl http://localhost:9200

echo Installing logstash
#http://logstash.net/docs/1.4.1/tutorials/getting-started-with-logstash
curl -O https://download.elasticsearch.org/logstash/logstash/logstash-1.4.1.tar.gz
tar zxvf logstash-1.4.1.tar.gz
cd logstash-1.4.1

echo done
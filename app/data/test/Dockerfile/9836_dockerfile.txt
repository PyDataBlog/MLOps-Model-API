# ┌────────────────────────────────────────────────────────────────────┐
# │ ElasticSearch Dockerfile                                           │
# ├────────────────────────────────────────────────────────────────────┤
# │ Copyright © 2014 Jordan Schatz                                     │
# │ Copyright © 2014 Noionλabs (http://noionlabs.com)                  │
# ├────────────────────────────────────────────────────────────────────┤
# │ Licensed under the MIT License                                     │
# └────────────────────────────────────────────────────────────────────┘

# This based on Java 8, see these notes about compatible java versions:
# http://www.elasticsearch.org/guide/en/elasticsearch/hadoop/current/requirements.ht ml
# http://www.elasticsearch.org/blog/java-1-7u55-safe-use-elasticsearch-lucene/
# http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/setup.html

# https://github.com/shofetim/docker-java
FROM shofetim/java:8

MAINTAINER Jordan Schatz "jordan@noionlabs.com"

# Install ElasticSearch.

# shofetim/java:8 is based on Debian Wheezy which doesn't have ES's
# cert, so skip the certificate check.

# To minimize image size, we run this as a single layer, and clean up
# after ourselves.

RUN \
  cd /tmp && \
  wget --no-check-certificate https://download.elasticsearch.org/elasticsearch/elasticsearch/elasticsearch-1.3.2.tar.gz && \
  tar xvzf elasticsearch-1.3.2.tar.gz && \
  rm -f elasticsearch-1.3.2.tar.gz && \
  mv /tmp/elasticsearch-1.3.2 /elasticsearch

# Define default command.
CMD ["/elasticsearch/bin/elasticsearch"]

# Expose ports.
#   - 9200: HTTP
#   - 9300: transport
EXPOSE 9200
EXPOSE 9300

# You will most likely want to do a combination of: pass environment
# variables, use a custom config. and map a host directory to store
# date. For guidance see:
# http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/setup-configuration.html
# and the fig.yml file in this directory

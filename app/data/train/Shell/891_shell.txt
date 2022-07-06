#!/bin/bash
# general good practice (stop on error, missing variables):
set -eu

chmod -R 777 $APACHE_LOG_DIR && \
exec /usr/bin/supervisord
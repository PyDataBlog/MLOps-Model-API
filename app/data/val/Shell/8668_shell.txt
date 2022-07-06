#!/bin/sh -e

# psql --variable=ON_ERROR_STOP=1 --username "$POSTGRES_USER" -c 'CREATE DATABASE postgres'
# psql --variable=ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname=main -c 'CREATE EXTENSION "uuid-ossp"'
# psql --variable=ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname=main -c 'CREATE EXTENSION "hstore"'
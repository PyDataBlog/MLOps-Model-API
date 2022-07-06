echo "Setup postgres"
sudo -u postgres psql <<< "
CREATE DATABASE blogdb;
CREATE USER blog1 WITH PASSWORD 'blogpassword';
GRANT ALL PRIVILEGES ON DATABASE blogdb TO blog1;
"

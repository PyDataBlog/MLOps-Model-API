echo Get latest 32bits zip files from https://dev.mysql.com/downloads/connector/c/
rem wget --no-check-certificate https://dev.mysql.com/get/Downloads/Connector-C/mysql-connector-c-6.1.6-win32.zip -O mysql\mysql.zip
rem cd mysql
rem ..\7zip\7z x mysql.zip
gem install mysql2 -v '0.4.1' --platform=ruby -- '--with-mysql-dir="%CD%\mysql\mysql-connector-c-6.1.6-win32"'

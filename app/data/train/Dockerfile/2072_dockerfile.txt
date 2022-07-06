FROM quay.io/panubo/php-apache-c7

RUN cd /var/www/html && git clone https://github.com/voltgrid/voltgrid-wordpress-example.git .

COPY Mountfile /var/www/html/Mountfile
COPY voltgrid.conf /usr/local/etc/voltgrid.conf

ADD https://raw.githubusercontent.com/voltgrid/voltgrid-pie/master/voltgrid.py /usr/local/bin/voltgrid.py
RUN chmod 755 /usr/local/bin/voltgrid.py && chmod 644 /usr/local/etc/voltgrid.conf

FROM registry.access.redhat.com/rhscl/httpd-24-rhel7

ENV HTTPD_ROOT_DIR /opt/rh/httpd24/root
ADD www/* ${HTTPD_ROOT_DIR}/var/www/html/
ADD virtualhosts/* ${HTTPD_ROOT_DIR}/etc/httpd/conf.d/

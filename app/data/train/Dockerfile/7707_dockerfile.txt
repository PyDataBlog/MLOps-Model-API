FROM centos
RUN yum install -y stunnel
ADD stunnel.conf /etc/stunnel/stunnel.conf
ADD stunnel.pem /etc/stunnel/stunnel.pem
CMD /usr/bin/stunnel /etc/stunnel/stunnel.conf

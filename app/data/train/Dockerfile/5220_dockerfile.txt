FROM pataquets/nginx-tls-proxy:1.9.4

RUN \
  mv -v \
  /etc/nginx/confs-available/tls-port-listen-spdy \
  /etc/nginx/conf.d/includes/tls-port-listen

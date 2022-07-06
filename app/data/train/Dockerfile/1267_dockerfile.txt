FROM nginx:1.9.4
MAINTAINER  Arthur Tsang <arthur_tsang@hotmail.com>

ADD     https://github.com/hashicorp/consul-template/releases/download/v0.10.0/consul-template_0.10.0_linux_amd64.tar.gz /tmp/consul-template.tgz
ADD     nginx.conf.ctmpl nginx.conf.ctmpl
RUN     cd /bin && gzip -dc /tmp/consul-template.tgz | tar -xf - && rm /tmp/consul-template.tgz && mv /bin/consul-template_0.10.0_linux_amd64/consul-template /bin/consul-template && rmdir /bin/consul-template_0.10.0_linux_amd64
RUN     apt-get update && apt-get install -y net-tools sharutils curl

ADD     entrypoint.sh entrypoint.sh
RUN     chmod a+x entrypoint.sh

EXPOSE  80
ENTRYPOINT ["./entrypoint.sh"]

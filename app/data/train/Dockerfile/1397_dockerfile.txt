# VERSION 0.4.0
# AUTHOR:         Nicolas Lamirault <nicolas.lamirault@gmail.com>
# DESCRIPTION:    portefaix/clair

FROM portefaix/base:3.2
MAINTAINER Nicolas Lamirault <nicolas.lamirault@gmail.com>

ENV VAULT_VERSION 0.4.0

RUN apk add --update build-base curl

RUN cd /tmp/ && \
    mkdir /etc/vault && \
	curl -LO https://releases.hashicorp.com/vault/${VAULT_VERSION}/vault_${VAULT_VERSION}_linux_amd64.zip && \
    unzip vault_${VAULT_VERSION}_linux_amd64.zip && \
    mv vault /usr/bin/ && \
	rm /tmp/vault_${VAULT_VERSION}_linux_amd64.zip && \
    apk del --purge build-base curl && \
    rm -rf /go /tmp/* /var/cache/apk/* /root/.n*

COPY ./dev.hcl /etc/vault/default.hcl

VOLUME ["/etc/vault"]

EXPOSE 8200

CMD ["/usr/bin/vault", "server", "-config=/etc/vault/default.hcl"]

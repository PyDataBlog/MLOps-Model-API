FROM ubuntu:18.04 

MAINTAINER Ron Kurr <kurr@jvmguy.com>

ADD https://releases.hashicorp.com/terraform/0.11.11/terraform_0.11.11_linux_amd64.zip /tmp/terraform.zip

RUN apt-get update && \
    apt-get install -y unzip ca-certificates git && \
    unzip /tmp/terraform.zip -d /usr/local/bin && \
    apt-get autoremove -y && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* && \
    rm -rf /tmp/*

RUN chmod 0555 /usr/local/bin/*

ENTRYPOINT ["/usr/local/bin/terraform"]
CMD ["--version"]

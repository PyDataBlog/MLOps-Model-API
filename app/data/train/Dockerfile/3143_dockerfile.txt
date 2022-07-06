FROM fedora:latest
MAINTAINER alex@alexblackie.com

RUN dnf install -y java-1.8.0-openjdk maven gradle && \
	dnf clean all

RUN useradd -m developer
USER developer

RUN mkdir -p /home/developer/.m2
COPY dev_java/settings.xml /home/developer/.m2/settings.xml

USER root

VOLUME /data

ADD dockerinit.sh /usr/local/bin/dockerinit.sh

CMD ["/bin/bash"]
ENTRYPOINT ["/usr/local/bin/dockerinit.sh"]

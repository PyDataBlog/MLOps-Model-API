FROM abm/webapp:v3
EXPOSE 80 5432
USER california
SHELL ["/bin/bash", "-l", "-c"]
ENV APP_HOME /home/california/app
WORKDIR $APP_HOME

ENV container docker
ENV RAILS_ENV production

RUN echo "export RAILS_ENV=production" >> /home/california/.bashrc
RUN echo "export CONTAINER=docker" >> /home/california/.bashrc
RUN echo "export TERM=xterm"

VOLUME [ "/sys/fs/cgroup", "/var/lib/redis/", "/var/lib/postgres/data" ]

USER root
ENTRYPOINT ["/usr/sbin/init", "--system"]

FROM patavi/build-ready

ADD patavi /var/lib/patavi/src
RUN chown -R patavi.patavi /var/lib/patavi/src
RUN su - patavi -c 'cd src/common && lein install'

RUN su - patavi -c 'cd src/worker && lein with-profile production deps'
RUN su - patavi -c 'cd src/server && lein with-profile production deps'
RUN su - patavi -c 'cd src/server && lein with-profile production install'
RUN su - patavi -c 'cd src/server-cached && lein with-profile production deps'

ADD patavi-server.sh /var/lib/patavi/bin/patavi-server
ADD patavi-worker.sh /var/lib/patavi/bin/patavi-worker
RUN chown patavi.patavi /var/lib/patavi/bin/patavi-*
RUN su - patavi -c 'chmod +x bin/patavi-server bin/patavi-worker'

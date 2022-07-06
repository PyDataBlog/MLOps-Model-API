FROM java:jre
MAINTAINER "Nathaniel Rankin Webb" <nrwebb@live.com>

ENV HOME /opt/jetbrains/hub
ENV VERSION 1.0
ENV BUILD ${VERSION}.749
ENV ZIP hub-ring-bundle${BUILD}.zip

WORKDIR $HOME
RUN wget --progress bar:force:noscroll https://download.jetbrains.com/hub/$VERSION/$ZIP && \
    unzip $ZIP -d . && \
    rm $ZIP

VOLUME ["$HOME"]
EXPOSE 8080
ENTRYPOINT ["bin/hub.sh"]
CMD ["run"]

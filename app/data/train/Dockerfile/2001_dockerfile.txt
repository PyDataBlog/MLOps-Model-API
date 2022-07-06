FROM meteorhacks/meteord:base
MAINTAINER Olivers De Abreu

ONBUILD COPY ./ /app
ONBUILD RUN bash $METEORD_DIR/on_build.sh
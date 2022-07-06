FROM buildpack-deps:jessie

RUN apt-get update \
      && apt-get install -y curl gettext wget \
      && rm -rf /var/lib/apt/lists/*

ENV MONO_VERSION 3.6.0

RUN mkdir /usr/src/mono \
      && curl -SL "http://download.mono-project.com/sources/mono/mono-$MONO_VERSION.tar.bz2" \
        | tar -xjC /usr/src/mono --strip-components=1 \
      && cd /usr/src/mono \
      && ./configure \
      && make get-monolite-latest \
      && make EXTERNAL_MCS="${PWD}/mcs/class/lib/monolite/basic.exe" \
      && make install \
      && rm -rf /usr/src/mono

ENV LD_LIBRARY_PATH /usr/local/lib

FROM feduxorg/centos
MAINTAINER dev@fedux.org

ENV RUBY_MAJOR 2.3
ENV RUBY_VERSION 2.3.1

# Build environment for ruby and ruby gems
RUN yum install -y gcc make

# Build ruby
RUN yum install -y openssl-devel libyaml libyaml-devel libffi libffi-devel readline-devel zlib zlib-devel gdbm gdbm-devel ncurses ncurses-devel bzip2 automake autoconf \
  && mkdir -p /usr/src/ruby \
  && curl -SL "https://cache.ruby-lang.org/pub/ruby/$RUBY_MAJOR/ruby-$RUBY_VERSION.tar.gz" \
  | tar -xzC /usr/src/ruby --strip-components=1 \
  && cd /usr/src/ruby \
  && autoconf \
  && ./configure --disable-install-doc \
  && make -j"$(nproc)" \
  && make install \
  && (yum remove -y openssl-devel libyaml-devel libffi-devel readline-devel zlib-devel gdbm-devel ncurses-devel bzip2 automake autoconf || exit 0) \
  && yum clean -y all \
  && rm -r /usr/src/ruby \
  # Do not create documentation on gem install
  && echo -e "install: --no-ri --no-rdoc\nupdate: --no-ri --no-rdoc" >> /usr/local/etc/gemrc

# Install bundler
ENV GEM_HOME /usr/local/bundle
ENV PATH $GEM_HOME/bin:$PATH
RUN gem install bundler \
      && bundle config --global path "$GEM_HOME" \
      && bundle config --global bin "$GEM_HOME/bin" \
      && rm -rf /usr/local/bundle/cache/*.gem

# Do not create .bundle in apps directory
ENV BUNDLE_APP_CONFIG $GEM_HOME

# Add defaults file for ruby
ADD ruby.conf /etc/default/ruby.conf
ADD profile.d-ruby.sh /etc/profile.d/ruby.sh

# Make sure all environment variables set for ruby are loaded
RUN sed -ie '/\[Service\]/aEnvironmentFile=/etc/default/ruby.conf' /etc/systemd/system/script-runner.service

WORKDIR /

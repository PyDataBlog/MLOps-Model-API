FROM geekaholic/rbenv
MAINTAINER Bud Siddhisena "bud@geekaholic.org"

ENV RUBY_VERSION 2.2.4

# Install default ruby, bundler
RUN . ~/.bashrc && rbenv install $RUBY_VERSION && rbenv global $RUBY_VERSION && \
    gem install --no-ri --no-rdoc bundler

WORKDIR /app

CMD ["/bin/bash"]


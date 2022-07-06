FROM ruby:2.2
ENV RAILS_VERSION 5.0.0.beta1

RUN apt-get update && \
    apt-get install -y mysql-client postgresql-client sqlite3 nodejs --no-install-recommends && \
    rm -rf /var/lib/apt/lists/*

RUN gem install rails --version "$RAILS_VERSION"

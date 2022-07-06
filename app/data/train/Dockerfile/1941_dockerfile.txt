FROM ruby:2.4.2-alpine3.6

WORKDIR /app

ENV LANG C.UTF-8

ENV DEL_PACKAGES \
      build-base \
      g++ \
      gcc \
      libxml2-dev \
      make

ENV KEEP_PACKAGES \
      libxslt-dev \
      sqlite-dev

# Add app dependencies config
COPY Gemfile Gemfile.lock ./

# Install dependencies
RUN bundle config --global --jobs `expr $(grep processor /proc/cpuinfo | wc -l) - 1` && \
    bundle config build.nokogiri --use-system-libraries && \
    apk update && \
    apk add ${KEEP_PACKAGES} ${DEL_PACKAGES} && \
    bundle install --deployment --without test development && \
    apk del ${DEL_PACKAGES} && \
    rm -r /var/cache/apk/*

# Add application source
ENV RAILS_ENV=production RAILS_LOG_TO_STDOUT=true
EXPOSE 3000
COPY . /app

CMD [ "rails", "server" ]
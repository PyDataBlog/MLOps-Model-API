#
# Besiege, simple web frontend for Siege.
#

FROM ubuntu:saucy

MAINTAINER Eric Platon <eplaton@gmx.com>

RUN apt-get update
RUN apt-get install -y siege git ruby2.0 ruby2.0-dev build-essential

RUN git clone git://github.com/ic/besiege.git
RUN gem install bundler --no-rdoc --no-ri
RUN cd /besiege && \
    bundle install

EXPOSE 9292
WORKDIR /besiege
CMD bundle exec rackup


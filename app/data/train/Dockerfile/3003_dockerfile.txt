FROM amclain/rbenv
MAINTAINER Alex McLain <alex@alexmclain.com>
RUN apt-get -qq update
RUN apt-get -y install build-essential libssl-dev
RUN rbenv install 2.1.2 -v
RUN rbenv global 2.1.2
RUN echo "gem: --no-rdoc" > ~/.gemrc
RUN gem update --force
RUN gem install pry rb-readline
CMD /bin/bash

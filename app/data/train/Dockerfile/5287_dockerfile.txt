FROM quay.io/aptible/debian

# Install general dependencies (git, build-essential)
RUN apt-install build-essential git

# Install Ruby and Bundler
RUN apt-install zlib1g-dev libssl-dev libreadline6-dev libyaml-dev
RUN cd /tmp && \
    wget -q http://cache.ruby-lang.org/pub/ruby/2.1/ruby-2.1.5.tar.gz && \
    tar xzf ruby-2.1.5.tar.gz && \
    cd ruby-2.1.5 && ./configure --enable-shared --prefix=/usr && \
    make && make install && cd .. && rm -rf ruby-2.1.5*
RUN gem install -N bundler

# Install NPM and Node.js
RUN cd /usr && \
    wget http://nodejs.org/dist/v0.10.33/node-v0.10.33-linux-x64.tar.gz && \
    tar --strip-components 1 -xzf node-v0.10.33-linux-x64.tar.gz && \
    rm -f node-v0.10.33*

# Install Python
RUN apt-install python3-minimal
RUN ln -s python3 /usr/bin/python

ADD test /tmp/test
RUN bats /tmp/test

FROM gcr.io/google_appengine/python-compat
MAINTAINER Grow SDK Authors <hello@grow.io>

# Update system.
RUN apt-get update
RUN apt-get upgrade -y

# Install Grow dependencies.
RUN apt-get install -y --no-install-recommends \
  python python-pip build-essential python-all-dev zip \
  libc6 libyaml-dev libffi-dev libxml2-dev libxslt-dev libssl-dev
RUN apt-get install -y --no-install-recommends git curl ssh

# Set environment variables.
ENV TERM=xterm
ENV GROW_VERSION=0.0.53

# Install Grow.
# We cannot use the grow distributable, we must compile from source so that we get a version
# with dependencies compiled against the older glibc library present in the python-compat image.
RUN mkdir -p /usr/local/src/grow/
WORKDIR /usr/local/src/grow/
RUN git clone --depth 50 https://github.com/grow/pygrow.git .
RUN git checkout tags/${GROW_VERSION}
RUN pip install --upgrade pip
RUN pip install --upgrade distribute
RUN pip install -r requirements.txt
RUN ln -s /usr/local/src/grow/bin/grow /usr/bin/grow

# Clean up APT when done.
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Make sure grow is installed correctly on the system path:
RUN grow --help

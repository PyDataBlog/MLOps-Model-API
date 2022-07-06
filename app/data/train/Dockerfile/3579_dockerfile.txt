FROM stackbrew/ubuntu:trusty
MAINTAINER b@heyomayeah.com

# ensure we are up to date
RUN apt-get update -qq

# install git
RUN apt-get install -qqy git

# install docker
ADD https://get.docker.io/builds/Linux/x86_64/docker-latest /usr/local/bin/docker
RUN chmod +x /usr/local/bin/docker

# install wrapdocker
ADD https://raw.githubusercontent.com/jpetazzo/dind/master/wrapdocker /usr/local/bin/wrapdocker
RUN chmod +x /usr/local/bin/wrapdocker

# install python and pip
RUN apt-get install -qqy python python-dev python-pip

# install virtualenv
RUN pip install virtualenv

VOLUME /var/lib/docker


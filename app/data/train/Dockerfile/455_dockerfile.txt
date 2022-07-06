#  Install PIP and setuptools

#  Version 0.1

FROM 		johnsn/python27
MAINTAINER	Nicholas Johns "nicholas.a.johns5@gmail.com"

RUN		apt-get update
RUN		apt-get install -y -q curl wget

RUN		curl https://bitbucket.org/pypa/setuptools/raw/bootstrap/ez_setup.py | python
RUN		curl https://raw.github.com/pypa/pip/master/contrib/get-pip.py | python

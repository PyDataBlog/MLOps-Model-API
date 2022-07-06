FROM metocean/aroha
MAINTAINER Greg Chalmers <g.chalmers@metocean.co.nz>

EXPOSE 5555

ADD . /install/
RUN /install/install.sh
CMD ["/sbin/initsh"]

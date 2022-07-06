## Data container providing persistant storage through a stopped container
# This should have the user associated with the client application(s) and be the same operating system where possible
# build: docker build -t jenkins-data .
# run  : docker run -d --name jenkins-data jenkins-data
FROM debian:jessie
MAINTAINER Ben Moorhouse (Ben@BenslittleOffice.com)

RUN useradd -d "/var/jenkins_home" -u 1000 -m -s /bin/bash jenkins
RUN mkdir -p /var/log/jenkins
RUN chown -R jenkins:jenkins /var/log/jenkins
VOLUME ["/var/jenkins_home"]
USER jenkins
# just a quick purpose reminder
CMD ["echo", "Data container for Jenkins"]

FROM phusion/passenger-customizable:0.9.17

# Set correct environment variables.
ENV HOME /root

# Use baseimage-docker's init process.
CMD ["/sbin/my_init"]

# Run apt-get update to be able to install apt-get packages
RUN apt-get update

# Install python & requirements
RUN apt-get install -y python3-pip

# Install requirements
ADD clans/requirements.txt /tmp/requirements.txt
RUN pip3 install -r /tmp/requirements.txt

# Clean up APT when done.
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Enable nginx
RUN rm -f /etc/service/nginx/down
RUN rm /etc/nginx/sites-enabled/default
ADD conf /etc/nginx/sites-enabled

# Install our code into /faf
ADD . /faf

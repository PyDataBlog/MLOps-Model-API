# Debian: PHP 5 (+ FANN)
#
# VERSION 0.0.1

# Pull the base image.
FROM ericmdev/php5

# Set the author.
MAINTAINER Eric Mugerwa <dev@ericmugerwa.com>

# Set environment variables.
ENV FILES config/

# Install packages.
RUN apt-get update && apt-get install -y \
    libfann-dev

# Install pecl fann.
RUN pecl install fann

# Add managed php ini files.
ADD ${FILES}/fann.ini /etc/php5/cli/conf.d/

# Configure executable.
ENTRYPOINT ["/bin/bash"]

# Define default command.
CMD []
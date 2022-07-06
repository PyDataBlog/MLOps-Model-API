FROM banovo/php:5

# Set environmental variables
ENV COMPOSER_HOME /root/composer
ENV PATH $COMPOSER_HOME/vendor/bin:$PATH

# Install composer
RUN set -xe \
	&& apt-get update && apt-get install -y git --no-install-recommends && rm -r /var/lib/apt/lists/* \
	&& curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

# Install prestissimo for faster composer installations
RUN composer global require "hirak/prestissimo:^0.3"

# Install phpunit
RUN composer global require phpunit/phpunit 5.3.*

COPY docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]

CMD ["php", "-a"]

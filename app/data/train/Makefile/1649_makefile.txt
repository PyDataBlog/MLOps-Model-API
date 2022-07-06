all: test

test:
	@cd /common-php
	@./vendor/bin/phpunit

test-coverage:
	@cd /common-php
	@./vendor/bin/phpunit --configuration /common-php/phpunit.xml --coverage-clover /common-php/coverage.clover
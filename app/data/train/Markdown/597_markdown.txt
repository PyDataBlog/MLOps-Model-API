# Blog based on symfony

### Requirement

- php

## Installation

```bash
$ git clone git@github.com:athlonUA/symfony-simple-blog.git
```

## Running

```bash
$ composer install
$ php app/console do:dat:cr
$ php app/console doctrine:schema:create
$ php app/console doctrine:fixtures:load
$ php app/console ass:in --symlink
$ php app/console ass:du
$ php app/console server:run
```

Go to the http://127.0.0.1:8000/ to run surf

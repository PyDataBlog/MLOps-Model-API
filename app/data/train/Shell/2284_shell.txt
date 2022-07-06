#!/usr/bin/bash

heroku pg:reset DATABASE_URL
heroku run rake db:setup

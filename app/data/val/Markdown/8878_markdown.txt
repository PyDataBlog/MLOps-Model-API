Grunt-Boilerplate
=================

A simple grunt project template that I use to build quick grunt projects with the same pattern.

# Install steps

This requires that you install [NodeJS](http://nodejs.org) and have the npm command available.

First you need to get the boilerplate code down to your machine:

	$ git clone git@github.com:acolchado/Grunt-Boilerplate.git

You can choose to install grunt globally with this command:

	$ npm install -g grunt-cli

Install the dependencies:

	$ npm install

#Customizing

Now that you have it installed locally you can use it to to create a new set of files that you can use in your own repo to start it off. Run this step:

	$ grunt package

This will create a directory called _package/. You can move the contents of this directory into your project's directory to use it as a starting place. Just be careful that you don't override files you already have, such as .gitignore, etc.


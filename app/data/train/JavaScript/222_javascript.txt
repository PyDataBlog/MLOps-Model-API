// package metadata file for Meteor.js

Package.describe({
  name: 'startup-cafe',
  "author": "Dragos Mateescu <dmateescu@tremend.ro>",
  "licenses": [
    {
      "type": "MIT",
      "url": "http://opensource.org/licenses/MIT"
    }
  ],
  "scripts": {

  },
  "engines": {
    "node": ">= 0.10.0"
  },
  "devDependencies": {
    "grunt": "~0.4.5",
    "grunt-autoprefixer": "~0.8.1",
    "grunt-contrib-concat": "~0.4.0",
    "grunt-contrib-jshint": "~0.10.0",
    "grunt-contrib-less": "~0.11.3",
    "grunt-contrib-uglify": "~0.5.0",
    "grunt-contrib-watch": "~0.6.1",
    "grunt-contrib-clean": "~ v0.6.0",
    "grunt-modernizr": "~0.5.2",
    "grunt-stripmq": "0.0.6",
    "grunt-wp-assets": "~0.2.6",
    "load-grunt-tasks": "~0.6.0",
    "time-grunt": "~0.3.2",
    "grunt-bless": "^0.1.1"
  }
});

Package.onUse(function (api) {
  api.versionsFrom('METEOR@1.0');
  api.use('jquery', 'client');
  api.addFiles([
    'dist/fonts/glyphicons-halflings-regular.eot',
    'dist/fonts/glyphicons-halflings-regular.svg',
    'dist/fonts/glyphicons-halflings-regular.ttf',
    'dist/fonts/glyphicons-halflings-regular.woff',
    'dist/fonts/glyphicons-halflings-regular.woff2',
    'dist/css/bootstrap.css',
    'dist/js/bootstrap.js',
  ], 'client');
});

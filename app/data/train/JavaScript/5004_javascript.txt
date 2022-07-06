Package.describe({
  name: 'craigslist-utils',
  summary: 'Npm Craigslist-utils packaged for Meteor.'
});

Npm.depends ({
  'craigslist-utils': '0.0.7'
});

Package.on_use(function (api) {

  api.add_files('craigslist-utils.js', ['server']);

  api.export('CL');

});

Package.on_test(function (api) {
  api.use('craigslist-utils');
  api.use('tinytest');
  
  api.add_files('craigslist-utils_tests.js');
});

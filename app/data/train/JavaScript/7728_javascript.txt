require('newrelic');

var express = require('express');
var app = express();
var http = require("http");

app.use(express.static(__dirname));
module.exports = app;

var server = app.listen(process.env.PORT || 3000, function() {
  console.log('express server listening on port ' + server.address().port);
});


setInterval(function() {
    http.get("http://cherrystreet.herokuapp.com");
}, 300000); 
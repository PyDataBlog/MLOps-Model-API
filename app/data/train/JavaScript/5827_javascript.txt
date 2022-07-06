
const Hapi = require('hapi');
const Request = require('request');

const port = process.env.PORT || 8080;
const server = new Hapi.Server();
const cephalopods = 'http://api.gbif.org/v1/species/136';

server.connection({
    port: port,
    host: '0.0.0.0'
});

server.route({
    method: 'GET',
    path: '/',
    handler: function (request, reply) {
        Request(cephalopods, function (err, response, body) {
            return reply(body);
        });
    }
});

server.start(function () {
    console.log('Server started on ' + port);
});

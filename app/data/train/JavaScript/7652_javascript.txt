var async = require('async'),
  awsSDK = require('aws-sdk'),
  uuid = require('node-uuid');

function client() {
  return new awsSDK.DynamoDB().client;
}

function putItem(done) {
  var item = {
    TableName: "test.performance.ssl",
    Item: {
      id: {
        S: uuid.v1()
      }
    }
  };

  client().putItem(item, done);
};

function put10(done) {
  var i = 10;
  var t = process.hrtime();
  async.whilst(function() {
    return i > 0;
  }, function(cb) {
    --i;
    return putItem(cb);
  }, function(e) {
    t = process.hrtime(t);
    console.log('%d seconds', t[0] + t[1] / 1000000000);
    return done(e);
  });
};

describe('ssl tests', function() {
  this.timeout(10000);
  describe('with ssl', function() {
    before(function() {
      awsSDK.config.update({
        accessKeyId: process.env.AWS_ACCESS_KEY,
        secretAccessKey: process.env.AWS_SECRET_KEY,
        sslEnabled: true,
        region: 'us-east-1'
      });
    });

    it('can put 10 items', put10);
  });
  describe('without ssl', function() {
    before(function() {
      awsSDK.config.update({
        accessKeyId: process.env.AWS_ACCESS_KEY,
        secretAccessKey: process.env.AWS_SECRET_KEY,
        sslEnabled: false,
        region: 'us-east-1'
      });
    });

    it('can put 10 items', put10);
  });
});

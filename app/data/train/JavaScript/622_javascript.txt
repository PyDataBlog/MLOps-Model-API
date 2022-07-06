var AWS = require('aws-sdk');
var Policy = require("./s3post").Policy;
var helpers = require("./helpers");
var POLICY_FILE = "policy.json";
var schedule = require('node-schedule');




var Worker = function(sqsCommnad, s3Object, simpleData){
	var queue = sqsCommnad;
	var s3 = s3Object;
	var simpleDataAuth = simpleData;
	
	var policyData = helpers.readJSONFile(POLICY_FILE);
	var policy = new Policy(policyData);
	
	var bucket_name = policy.getConditionValueByKey("bucket");
	
	
	
	Worker.prototype.job = function(){
		
		var run = schedule.scheduleJob('*/4 * * * * *',
			function(){
				
				queue.recv(function(err, data){
					if (err) { 
						console.log(err); 
						return; 
					}
					
					console.log({Body : data.Body, MD5OfBody : data.MD5OfBody});
					
					params = {
						Bucket: bucket_name,
						Key: data.Body
					}
					
					s3.getObject(params, function(err, data) {
						if (err) {
							console.log(err, err.stack);
						}
						else {
							var request = require('request');
							var mime = require('mime');

							
							var gm = require('gm').subClass({ imageMagick: true });						
							
							var src = 'http://s3-us-west-2.amazonaws.com/'+params.Bucket+'/'+params.Key;
							
							
							
							gm(request(src, params.Key))
							.rotate('black', 15)
							.stream(function(err, stdout, stderr) {
								var buf = new Buffer('');
								stdout.on('data', function(res) {
									buf = Buffer.concat([buf, res]);
								});
								stdout.on('end', function(data) {
									var atr = {
										Bucket: params.Bucket,
										Key: params.Key,
										Body: buf,
										ACL: 'public-read',
										Metadata: {
											"username" : "Szymon Glowacki",
											"ip" : "192.168.1.10"
										}										
									};
									s3.putObject(atr, function(err, res) {
										console.log("done");
									});
								});
							});
							
						}
					});
				});
			});
	}
}

module.exports = Worker;






var task =    function(request, callback, configuration){
        var template = "helloParamterized.ejs";
        var AWS = configuration.aws;
        var S3 = new AWS.S3();
        callback(null, {template: template, params:{info:"Hello World from code!"}});
    }



exports.action = task;

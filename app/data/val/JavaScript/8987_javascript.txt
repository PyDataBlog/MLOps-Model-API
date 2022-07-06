var gulp = require('gulp');
var karma = require('karma');
var fs = require('fs');

gulp.task('test', function() {
  new karma.Server({configFile: __dirname + '/karma.conf.js', reporters: 'dots'}).start();
  var content = fs.readFileSync('copyAndRenameTest.js');
  console.log(content);
});

'use strict';

module.exports = function(grunt) {

  // Please see the Grunt documentation for more information regarding task
  // creation: http://gruntjs.com/creating-tasks

  function random(min, max) {
    min = min || 0;
    max = max || 1;

    var result;
    if(min === 0 && max === 1) {
      result = Math.random();
    } else {
      result = Math.floor((Math.random() * max) + min);
    }
    return result;
  }

  grunt.registerMultiTask('random', 'Your task description goes here.', function() {
    // Merge task-specific and/or target-specific options with these defaults.
    var options = this.options({min:0,max:1});
    grunt.log.writeln('Random: ' + random(options.min, options.max));
  });

};
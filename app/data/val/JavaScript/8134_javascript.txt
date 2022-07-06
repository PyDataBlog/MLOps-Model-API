var chokidar = require('chokidar');

process.chdir(__dirname + '/../');
chokidar.watch('.', {ignoreInitial: true, useFsEvents: true}).on('all', function(event, path) {
    console.log(new Date, event, path);
});

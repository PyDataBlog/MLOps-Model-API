module.exports = function (process, manifest, actionCallback) {
    var program = require('commander');

    program
        .version(manifest.version)
        .option('-v, --verbose', 'Enable verbose output')
        .command('*')
        .action(function (args) {
            actionCallback(process, args);
        });

    program.parse(process.argv);
};
'use strict';

module.exports = {
    images: {
        files: [
            {
                cwd     : 'src/assets/img/',
                src     : '**/*',
                dest    : '.build/img/',
                flatten : false,
                expand  : true
            }
        ]
    },
    config: {
        files: [
            {
                cwd     : 'src/config/',
                src     : '**/*',
                dest    : '.build/',
                flatten : false,
                expand  : true
            }
        ]
    }
};

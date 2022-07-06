/// <reference path="Utility.ts" />
module Succintly {
    window.onload = function () {
        var formatter = new Utility.Formatter();
        var logger = new Utility.Logger(formatter);
        logger.log('Logger is loaded');
    };
}
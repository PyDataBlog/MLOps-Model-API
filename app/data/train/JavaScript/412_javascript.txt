"use strict";

var GLOBAL_MOUNT_POINT_MAX = Math.pow(2, 53);
var util = {
    uuid: function(){
        return Math.ceil(Math.random() * GLOBAL_MOUNT_POINT_MAX);
    }
};

module.exports = util;
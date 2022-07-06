;(function(){
'use strict'
const express = require('express');
const router = express.Router();

router.post('/', function(req, res){
    console.error('route: /create, ip: %s, time: %s', req.ip, new Date().toTimeString().substr(0,9));
    
    console.error(Object.keys(req));

    console.error(req.body);
    
    res.status(200).json('user created!');
});

module.exports = router;
})();
var express = require('express');
var router = express.Router();

/**
 * 获得主页内容
 *
 */
router.get('/', function(req, res, next) {
    res.render('user/index', {
        title: 'Express'
    });
});

module.exports = router;

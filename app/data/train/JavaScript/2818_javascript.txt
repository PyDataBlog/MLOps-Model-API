var express = require('express');
var user = require('../model/user');
var jsonReturn = require('../common/jsonReturn');
var router = express.Router();


/* GET users listing. */
router.get('/', function(req, res, next) {
  user.getUsers(function(err, rows, fields){
    res.render('users', { users : rows } );
  })
});

router.post('/getUsers', function (req, res, next) {
  if(req.body.username){
    user.getUsers(function(err, rows, fields){
      res.json(jsonReturn({ users : rows } ));
    })
  }
});

module.exports  = router;

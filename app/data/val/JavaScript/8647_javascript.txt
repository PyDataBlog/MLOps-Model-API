var express = require('express');
var router = express.Router();

/*
**	www.g-trotter.eu/my-trip
*/

router.route('/')
	.get(function(req, res) {
		res.json({
			path: 'www.g-trotter.eu/my-trip'
		})
	})

module.exports = router;
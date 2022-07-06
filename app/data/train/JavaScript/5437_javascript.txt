!(function(){
	
	var spg = require('./simple-password-generator.js');

	console.log(spg.generate({
		length : 3
	}));

})();
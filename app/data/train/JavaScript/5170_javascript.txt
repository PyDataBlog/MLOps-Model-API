var Counter = require('./counterEvented.js');

var c = new Counter(1);

c.increment();

c.on('even', function(){
	console.log('even! ' + c.count);
	return;
});
var fs = require('fs'),
	cons = require('consolidate'),
	dust = require('dustjs-linkedin');


var pages = [
	'index',
	'contact',
	'faq',
	'registration',
	'sponsors',
	'travel',
	'visit',
	'volunteers'
];

pages.forEach(function(page) {

	cons.dust('views/'+page+'.dust', { views: __dirname+'/views'}, function(err, html) {
		if(err) return console.log('error: ', err);

		fs.writeFile(__dirname+'/dist/'+page+'.html', html, function(err) {
			if(err) return console.log('error saving file: ', page, err);

			console.log('create page: ', page);
		});
	});

});

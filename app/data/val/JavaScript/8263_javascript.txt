var fs = require('fs');
/*var mulu = fs.readFileSync('./mulu.html','utf8');
var sutra = mulu.replace(/<sutra>/g,'#<sutra>').split(/#/);

var vol = function(sutra){
	for (var i in sutra){
		var vol = sutra[i].match(/<vol>(.+)<\/vol>/);
		var sutraid = sutra[i].match(/<sutraid n="(.+)"\/>/);
		if (vol == null || sutraid == null) {
			console.log('');
		} else {
			console.log(sutraid[1],vol[0]);
		}
	}
}
var cname = function(sutra){
	for (var i in sutra){
		var cname = sutra[i].match(/<taisho.+chi="(.+)\/">/g);
		var sutraid = sutra[i].match(/<sutraid n="(.+)"\/>/);
		if (cname == null || sutraid == null) {
			console.log('');
		} else {
			console.log(sutraid[1],cname.map(function(cname){
				var chiname = cname.replace(/<taisho.+chi="(.+)\/>/g,"$1");
				return chiname;	
			}));
		}
	}
}*/
///////////////////////////////////////////////////////////
fs.readdir('input',function(err,files){
	if (files[0] == ".DS_Store") files.splice(0,1);
	//console.log(files,files.length);
	files.map(persutra);
})


var persutra = function(fn){
	var text = fs.readFileSync('./input/'+fn,'utf8');
	var sutra = JSON.parse(text);
	console.log(sutraid(fn));
	console.log(findeachtag(sutra));
	
	
	//var sutra = arr[3]['མདོ་མིང་།'];	
	
	//console.log(sutra);
}
var sutraid = function(id){
	var sutraid = id.match(/J\d+((\([a-z]\))|(\,\d+))*/)[0];
	return ('<sutra>' + breakline + '<sutraid n="' + sutraid + '"/>'); 
}
var findeachtag = function(arr){
	var trans = 'translator';
	var tname = arr[3]['མདོ་མིང་།'];
	var aname = arr[3]['མདོ་མིང་གཞན།'];
	var sname = arr[3]['རྒྱ་གར་མདོ་མིང་།'];
	var homage = arr[3]['བསྒྱུར་ཕྱག'];
	var subject = arr[3]['བརྗོད་བྱ།'];
	var yana = arr[3]['ཐེག་པ།'];
	var charka = arr[3]['དཀའ། འཁོར་ལོ།'];
	var location = arr[3]['གནས་ཕུན་སུམ་ཚོགས་པ།'];
	var audience = arr[3]['འཁོར་ཕུན་སུམ་ཚོགས་པ།'];
	var aurthor = arr[3]['སྟོན་པ་ཕུན་སུམ་ཚོགས་པ།'];
	var requester = arr[3]['ཞུ་བ་པོ་ཕུན་སུམ་ཚོགས་པ།'];
	var dharma = arr[3]['ཆོས་ཕུན་སུམ་ཚོགས་པ།'];
	var purpose = arr[3]['ཆོས་ཀྱི་དགོས་དོན།'];
	var collect = arr[3]['བསྡུས་པའི་དོན། ལེའུ།'];
	var bampo = arr[3]['བམ་པོ། ཤོ་ལོ་ཀ'];
	var relation = arr[3]['མཚམས་སྦྱར་བའི་གོ་རིམ།'];
	var debate = arr[3]['རྒལ་ལན།'];
	var translator = arr[3]['ལོ་ཙཱ་བ།'];
	var reviser = arr[3]['ཞུ་དག་པ།'];
	//var cname = arr[3]['རྒྱ་ནག་མདོ་མིང་།'];
	if (arr[1]['ལོ་ཙཱ་བ།'] == 'translator_revisor' || arr[2]['ལོ་ཙཱ་བ།'] == 'translator_revisor') trans = 'translator_reviser';
	return "<tname>" + tname + "</tname>" + breakline + "<aname>" + aname + "</aname>" + breakline +
		   "<sname>" + sname + "</sname>" + breakline + "<cname></cname>" + breakline + "<vol></vol>" + breakline +
		   "<homage>" + homage + "</homage>" + breakline + "<subject>" + subject + "</subject>" + breakline +
		   "<yana>" + yana + "</yana>" + breakline + "<charka>" + charka + "</charka>" + breakline + "<location>" + 
		   location + "</location>" + breakline + "<audience>" + audience + "</audience>" + breakline + "<aurthor>" + 
		   aurthor + "</aurthor>" + breakline + "<requester>" + requester + "</requester>" + breakline + "<dharma>" +
		   dharma + "</dharma>" + breakline + "<purpose>" + purpose + "</purpose>" + breakline + "<collect>" + collect + 
		   "</collect>" + breakline + "<bampo>" + bampo + "</bampo>" + breakline + "<relation>" + relation + "</relation>" +
		   breakline + "<debate>" + debate + "</debate>" + breakline + "<" + trans + ">" + translator + "</" + trans + ">" +
		   breakline + "<reviser>" + reviser + "</reviser>" + breakline + "</sutra>";
}
var breakline = RegExp('\n');
/*var writexml = function(xml){
	fs.writeFileSync('bio.xml',JSON.stringify(xml,'',' '),'utf8');
}*/


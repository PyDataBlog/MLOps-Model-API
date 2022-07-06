//Create Tab Group

var tabGroup = Titanium.UI.createTabGroup();
 
// Variables

var Teas = ['#F5F5DC', '#FFE4B5', '#FFE4C4', '#D2B48C', '#C3b091', 
			'#c3b091', '#926F5B', '#804000', '#654321', '#3D2B1F'];
allRows = [];
 
var theColours = Ti.UI.createTableView({});
 
for (var i=0; i<Teas.length; i++){
       theRow = Ti.UI.createTableViewRow({backgroundColor:Teas[i], height:50, TeaColour:Teas[i]});
       allRows.push(theRow);
}
 
theColours.setData(allRows);

var options = Ti.UI.createView({layout: 'vertical'});

var showCamera = Ti.UI.createButton({title: 'Show Camera'});


// TeaSelection Function

function getVerdict(colour){
       var indicator = colour.charAt(1);
       var msg;
       switch(indicator){
              case 'F': msg = 'Milky'; break;
              case 'D': msg = 'Nice'; break;
              case 'C': msg = 'Perfect'; break;
              case '9': msg = 'A bit strong'; break;
              case '8': msg = 'Builders tea'; break;
              case '6': msg = 'Send it back'; break;
              case '3': msg = 'No milk here'; break;
       }
       return msg;
};
 
function showTeaVerdict(_args){
       var teaVerdict = Ti.UI.createWindow({layout:'vertical'});
       teaVerdict.backgroundColor = _args;
       teaVerdict.msg = getVerdict(_args);
       var judgement = Ti.UI.createLabel ({text:teaVerdict.msg, top:'50%'});
       var close = Ti.UI.createButton ({title:'Choose Again', top:'25%'});
       close.addEventListener('click', function(e){
              teaVerdict.close();
              teaVerdict = null;
       });
       teaVerdict.add(judgement);
       teaVerdict.add(close);
       teaVerdict.open();
}

//Camera Function

function showPhoto(_args) {
	thePhoto.setImage(_args.media);
}

 

// Tab 1

var winTea = Titanium.UI.createWindow({ 
    title:'Select Color',
    backgroundColor:'#fff'
});
var tabTea = Titanium.UI.createTab({ 
    title:'TeaSelection',
    window:winTea
});
 
winTea.add(theColours);
 
// Tab 2
var winCamera = Titanium.UI.createWindow({ 
    title:'Camera',
    backgroundColor:'#fff'
});
 
var tabCamera = Titanium.UI.createTab({ 
    title:'Camera',
    window:winCamera
});

winCamera.add(showCamera);

// Add Listener

theColours.addEventListener('click', function(e){showTeaVerdict(e.source.TeaColour);});
 showCamera.addEventListener('click', function (e) {
Ti.Media.showCamera({animated: true,
	                 autoHide: true,
	                 saveToPhotoGallery: true,
	                 showControls: true,
	                 mediaTypes: [Ti.Media.MEDIA_TYPE_PHOTO],
	                 success:    function(e) {showPhoto(e);}
	                });
});



// Add Tabs

tabGroup.addTab(tabTea); 
tabGroup.addTab(tabCamera);

// Open tabGroup

tabGroup.open();
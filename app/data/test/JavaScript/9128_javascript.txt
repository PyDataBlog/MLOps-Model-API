/*
 * Sample Window
 */
function SampleWindow(navController){
	/*
	 * essentials
	 */
	var win = Titanium.UI.createWindow({
		top: ULA_WIN_TOP,
		title: 'Sample Window',
		backgroundColor: 'red'
	});
	
	var rootView = Titanium.UI.createView({
		top: 10, bottom: 10, left: 10, right: 10,
		backgroundColor: 'blue',
		layout: 'vertical'
	});
	
	/*
	 * components
	 */
	var lblTitle = Titanium.UI.createLabel({
		text: 'This is Sample window',
		color: 'black',
		font: ULA_FONT_A,
		top: 100
	});
	
	var btnClose =Titanium.UI.createButton({
		title: 'Close',
		color: 'black',
		height: 60,
		width: 100,
		top: 20,
	});
	
	btnClose.addEventListener('click',function(e){
		_c('btnClosed clicked');
		navController.back(1);
	});
	
	/*
	 * heirarchy
	 */
	
	rootView.add(lblTitle);
	
	win.add(rootView);
	win.rightNavButton = btnClose;
	
	return win;
}

module.exports = SampleWindow;

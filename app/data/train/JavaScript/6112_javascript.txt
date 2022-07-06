(function ($) {
	
	$(document).ready(function() {
		var highestCol = Math.max($('.first-menu .pane-content').height(),$('.middle-menu .pane-content').height(),$('.last-menu .pane-content').height());
		
		/*.first-menu .pane-content, .middle-menu .pane-content, .last-menu .pane-content
		$('.elements').height(highestCol);*/
		alert(highestCol);
		
		});
	
})(jQuery);  

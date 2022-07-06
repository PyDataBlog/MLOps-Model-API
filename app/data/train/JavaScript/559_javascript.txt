$('.js-toggle-menu').click(function(e){
	e.preventDefault();
	$(this).siblings().toggle();
});
$('.nav--primary li').click(function(){
	$(this).find('ul').toggleClass('active');
});

    $('section').horizon();
	$(".menu-list").find("li").each(function(){
		var menuIndex = $(this).index()+1;
		$(this).click(function(){
			$(document).stop().horizon('scrollTo', menuIndex);
		})

	})
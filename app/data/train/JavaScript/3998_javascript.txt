/*!
 * Start Bootstrap - Agency Bootstrap Theme (http://startbootstrap.com)
 * Code licensed under the Apache License v2.0.
 * For details, see http://www.apache.org/licenses/LICENSE-2.0.
 */

// jQuery for page scrolling feature - requires jQuery Easing plugin
$(function() {
    $('a.page-scroll').bind('click', function(event) {
        var $anchor = $(this);
        $('html, body').stop().animate({
            scrollTop: $($anchor.attr('href')).offset().top
        }, 1500, 'easeInOutExpo');
        event.preventDefault();
    });
});

// Highlight the top nav as scrolling occurs
$('body').scrollspy({
    target: '.navbar-fixed-top'
})

// Closes the Responsive Menu on Menu Item Click
$('.navbar-collapse ul li a').click(function() {
    $('.navbar-toggle:visible').click();
});



$(window).on('scroll', function() {
    var y_scroll_pos = window.pageYOffset;

    $('.timeline-image').css('background-color', '#fed136').each(function(){
    	var element = $(this);
    	if(element.offset().top > y_scroll_pos + $(window).height()/3 && element.offset().top < y_scroll_pos + 2* $(window).height()/3){
    		element.css('background-color', '#9a0000');
    	}
    });

});
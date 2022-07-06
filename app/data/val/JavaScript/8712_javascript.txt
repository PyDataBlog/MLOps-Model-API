/**
 * @file
 * A JavaScript file for the theme.
 *
 * In order for this JavaScript to be loaded on pages, see the instructions in
 * the README.txt next to this file.
 */

// JavaScript should be made compatible with libraries other than jQuery by
// wrapping it with an "anonymous closure". See:
// - http://drupal.org/node/1446420
// - http://www.adequatelygood.com/2010/3/JavaScript-Module-Pattern-In-Depth
(function($, Drupal, window, document, undefined) {
    $(document).ready( function(){
       if($("#block-nice-menus-1").length > 0){ 
        $('#block-nice-menus-1').meanmenu({
         meanMenuContainer: '.region-header',
         meanScreenWidth: "1100",
         meanRemoveAttrs: true,
         meanDisplay: "table",
         meanMenuOpen: "<span></span><span></span><span></span>",
         meanRevealPosition: "left",
         meanMenuClose: "<span></span><span></span><span></span>"
        }); 
        jQuery("#block-block-10 .social-icon .search").click(function(event){
          event.preventDefault();
          jQuery("#block-search-form").fadeToggle("fast");
        });         
       }
       jQuery('#toggle-text').click(function () {
            if(jQuery('#content-text').hasClass('.open')){
              jQuery('#content-text').hide('slow').removeClass('.open');
              jQuery('#toggle-text').html('Read More');
              }else{
              jQuery('#content-text').addClass('.open').show('slow');
              jQuery('#toggle-text').html('Read Less');
            }
       });       
    });
// Place your code here.
})(jQuery, Drupal, this, this.document);



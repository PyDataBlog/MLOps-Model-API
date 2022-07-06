WP Fast Search
==============

"A blazingly fast drop-down search widget for Wordpress."

Responds to key press, instantly presents a drop-down menu of post titles for
the user to select from (with ten-post max per result set). 

The user can scroll through the list of posts using the mouse or keyboard 
(pressing enter or clicking on a title will take the user to the selected 
post.)

Data for posts are loaded asychronously once the page is loaded, and so
it will not affect the page loading speed. Only one request per page load 
is ever sent to the server, which queries for cached post information, and 
therefore the plugin is performant.

### Bugs/Todos

- Presents a white background for results, which might conflict with a
theme's color scheme.

- JavaScript and CSS are not cached (though they are quite small.)
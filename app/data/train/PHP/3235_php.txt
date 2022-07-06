<?php 
/*=================================================
	Disable Wordpress Autoformatting
==================================================*/
function convax_formatter($content) {
		$new_content = '';
		//Matches the contents and the open and closing tags
		$pattern_full = '{(\[raw\].*?\[/raw\])}is';
		//Matches just the contents
		$pattern_contents = '{\[raw\](.*?)\[/raw\]}is';
		//Divide content into pieces
		$pieces = preg_split($pattern_full, $content, -1, PREG_SPLIT_DELIM_CAPTURE);
		//Loop over pieces
		foreach ($pieces as $piece) {
		//Look for presence of the shortcode
		if (preg_match($pattern_contents, $piece, $matches)) {
		//Append to content (no formatting)
		$new_content .= $matches[1];
		} else {
		//Format and append to content
		$new_content .= wptexturize(wpautop($piece));
		}
		}
		return $new_content;
	}

/*=================================================
	Setting up the default thumbnail sizes
================================================== */

remove_filter('the_content', 'wpautop');
remove_filter('the_content', 'wptexturize');
// Before displaying for viewing, apply this function
add_filter('the_content', 'convax_formatter', 99);
add_filter('widget_text', 'convax_formatter', 99);

//Setting default thumbnail size
add_image_size('large', 500, 500, true);
add_image_size('medium', 200, 200, true);
add_image_size('thumbnail', 84, 84, true);

//Function to crop all thumbnails
if(false === get_option("medium_crop")) {
add_option("medium_crop", "1");
} else {
update_option("medium_crop", "1");
}
if(false === get_option("large_crop")) {
add_option("large_crop", "1");
} else {
update_option("large_crop", "1");
}
if(false === get_option("thumbnail_crop")) {
add_option("thumbnail_crop", "1");
} else {
update_option("thumbnail_crop", "1");
}

?>
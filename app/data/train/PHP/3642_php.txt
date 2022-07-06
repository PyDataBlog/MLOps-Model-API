
// ADDING JAVASCRIPT

 function fp_scripts()
 {
	// Register the script like this for a theme:
        // wp_register_script( $handle, $src, $deps, $ver, $in_footer );
        wp_register_script( 'custom-script', get_template_directory_uri() . '/js/custom-script.js', array( 'jquery', 'jquery-ui-core' ), '20131118', true );  
	// use the array to grab a library already included in WordPress, in this case 'jquery' and 'jquery-ui-core'

	// For either a plugin or a theme, you can then enqueue the script:
	wp_enqueue_script( 'custom-script' );
 }
 // add_action( $tag, $function_to_add, $priority, $accepted_args );
 add_action( 'wp_enqueue_scripts', 'fp_scripts' );



// ADDING STYLES

 function fp_styles()
 {
	// Register the style like this for a theme:
	// wp_register_style( $handle, $src, $deps, $ver, $media ); note "$media" instead of $in_footer
		// $media parameter can take 'all', 'screen', 'handheld', or 'print' values
	wp_register_style( 'custom-style', get_stylesheet_directory() . '/css/custom-style.css', array(), '20131118', 'all' );
           // note replacing get_template_directory_uri with get_stylesheet_directory_uri
           // http://codex.wordpress.org/Function_Reference/get_stylesheet_directory_uri
           // http://codex.wordpress.org/Child_Themes
           	// get_stylesheet_directory() points to your child theme's directory (not the parent theme's directory).
           // http://www.wpbeginner.com/wp-tutorials/how-to-properly-add-javascripts-and-styles-in-wordpress/
           	// If you are working with a child theme, then use get_stylesheet_directory_uri()
	// For either a plugin or a theme, you can then enqueue the style:
	wp_enqueue_style( 'custom-style' );
 }
 add_action( 'wp_enqueue_scripts', 'fp_styles' );


// from http://wp.tutsplus.com/articles/how-to-include-javascript-and-css-in-your-wordpress-themes-and-plugins/
// Agrument that you *should not* move scripts to the footer 
// --> http://code.tutsplus.com/articles/a-beginners-guide-to-enqueuing-jquery--wp-30615
// Also - load scripts only on plugin pages http://codex.wordpress.org/Function_Reference/wp_enqueue_script#Load_scripts_only_on_plugin_pages

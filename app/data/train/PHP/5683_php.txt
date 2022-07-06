<?php
/*
 *	This file contains "Header slider" section & fields.
 *	Contains: header slider options.
 *	
 *	@version 1.0
 *	@since rexly 1.0
 */
	
	/*
	 *	Section -- Header
	 *
	 *	@since rexly 1.0
	 */
	 
	rexly_Kirki::add_section( 'rexly_header_section', array(
	    'title'          	=> __( 'Header', 'rexly' ),
	    'priority'       	=> 9,
	    'capability'     	=> 'edit_theme_options',
	) );
	
	// Header section info
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'        		=> 'custom',
		'settings'    		=> 'rexly_header_section_info',
		'section'     		=> 'rexly_header_section',
		'default'     		=> '<div style="padding:5px;font-size:11px;background-color:#333333;text-align:center;color:#ffffff;border:1px solid #dddddd;"> '. __( 'Header section will appear only on frontpage & blog listing page.', 'rexly' ) .' </div>',
		'priority'    		=> 10,
	) );
	
	// enable/disable for frontpage
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'        		=> 'switch',
		'settings'    		=> 'rexly_header_section_frontpage_switch',
		'label'       		=> __( 'Disable header for frontpage', 'rexly' ),		
		'section'     		=> 'rexly_header_section',
		'default'     		=> '1',
		'priority'    		=> 10,
		'choices'     		=> array(
			'on'  				=> esc_attr__( 'On', 'rexly' ),
			'off' 				=> esc_attr__( 'Off', 'rexly' ),
		),
	) );
	
	// enable/disable for blog page
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'        		=> 'switch',
		'settings'    		=> 'rexly_header_section_blog_switch',
		'label'       		=> __( 'Disable header for blog pages', 'rexly' ),		
		'section'     		=> 'rexly_header_section',
		'default'     		=> '1',
		'priority'    		=> 10,
		'choices'     		=> array(
			'on'  				=> esc_attr__( 'On', 'rexly' ),
			'off' 				=> esc_attr__( 'Off', 'rexly' ),
		),
	) );	
	
	// header background image
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'        		=> 'image',
		'settings'    		=> 'rexly_header_bg_image',
		'label'       		=> __( 'Header Image', 'rexly' ),
		'section'     		=> 'rexly_header_section',
		'default'     		=> get_template_directory_uri() . '/img/header-bar-bg.jpg',
		'priority'    		=> 10,
        'transport'   		=> 'postMessage',
        'js_vars'     		=> array(
            array(
                'element'  		=> '.header-img',
                'function' 		=> 'html',
                'attr'     		=> 'src',
            ),
        ),			
	) );
	
	// background overlay color	 	
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'        		=> 'color',
		'settings'    		=> 'rexly_header_overaly',
		'label'       		=> __( 'Background overlay', 'rexly' ),
		'section'     		=> 'rexly_header_section',
		'default'     		=> 'rgba(0, 0, 0, 0.6)',
		'priority'    		=> 10,
		'alpha'       		=> true,		
		'transport'   		=> 'auto',
        'output'      		=> array(
            array(
                'element'  		=> '.header-overlay',
                'property' 		=> 'background-color',
            ),
        ),					
	) );
	
	// slide before text
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'     			=> 'text',
		'settings' 			=> 'rexly_slide_before_text',
		'label'    			=> __( 'Slide before text', 'rexly' ),
		'section'  			=> 'rexly_header_section',
		'default'  			=> __( 'we generate', 'rexly'),
		'priority' 			=> 10,
        'transport'   		=> 'postMessage',
        'js_vars'     		=> array(
            array(
                'element'  		=> '.before-header-text',
                'function' 		=> 'html',
            ),
        ),				
	) );
	
	// slide after text
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'     			=> 'text',
		'settings' 			=> 'rexly_slide_after_text',
		'label'    			=> __( 'Slide after text', 'rexly' ),
		'section'  			=> 'rexly_header_section',
		'default'  			=> __( 'Ideas', 'rexly' ),
		'priority' 			=> 10,
        'transport'   		=> 'postMessage',
        'js_vars'     		=> array(
            array(
                'element'  		=> '.after-header-text',
                'function' 		=> 'html',
            ),
        ),				
	) );
	
	// slide text info
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'        		=> 'custom',
		'settings'    		=> 'rexly_slide_text_info',
		'section'     		=> 'rexly_header_section',
		'default'     		=> '<div style="padding:5px;font-size:11px;background-color:#333333;text-align:center;color:#ffffff;border:1px solid #dddddd;"> '. __( 'Add slide texts', 'rexly' ) .' </div>',
		'priority'    		=> 10,
	) );
	
	// slide text 1
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'     			=> 'text',
		'settings' 			=> 'rexly_slide_text_1',
		'label'    			=> __( 'Slide text 1', 'rexly' ),
		'section'  			=> 'rexly_header_section',
		'default'  			=> __( 'Creative', 'rexly' ),
		'priority' 			=> 10,				
	) );
	
	// slide text 2
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'     			=> 'text',
		'settings' 			=> 'rexly_slide_text_2',
		'label'    			=> __( 'Slide text 2', 'rexly' ),
		'section'  			=> 'rexly_header_section',
		'default'  			=> __( 'Awesome', 'rexly' ),
		'priority' 			=> 10,				
	) );
	
	// slide text 3
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'     			=> 'text',
		'settings' 			=> 'rexly_slide_text_3',
		'label'    			=> __( 'Slide text 3', 'rexly' ),
		'section'  			=> 'rexly_header_section',
		'default'  			=> __( 'Valuable', 'rexly' ),
		'priority' 			=> 10,				
	) );	
	
	// slide text 4
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'     			=> 'text',
		'settings' 			=> 'rexly_slide_text_4',
		'label'    			=> __( 'Slide text 4', 'rexly' ),
		'section'  			=> 'rexly_header_section',
		'default'  			=> __( 'Great', 'rexly' ),
		'priority' 			=> 10,				
	) );
	
	// slide text 5
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'     			=> 'text',
		'settings' 			=> 'rexly_slide_text_5',
		'label'    			=> __( 'Slide text 5', 'rexly' ),
		'section'  			=> 'rexly_header_section',
		'default'  			=> __( 'Amazing', 'rexly' ),
		'priority' 			=> 10,				
	) );
	
	// button info
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'        		=> 'custom',
		'settings'    		=> 'rexly_slide_button_info',
		'section'     		=> 'rexly_header_section',
		'default'     		=> '<div style="padding:5px;font-size:11px;background-color:#333333;text-align:center;color:#ffffff;border:1px solid #dddddd;"> '. __( 'Add header buttons', 'rexly' ) .' </div>',
		'priority'    		=> 10,
	) );
	
	// button 1 text
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'     			=> 'text',
		'settings' 			=> 'rexly_slide_button_1_text',
		'label'    			=> __( 'Button 1 text', 'rexly' ),
		'section'  			=> 'rexly_header_section',
		'default'  			=> __( 'Know More', 'rexly' ),
		'priority' 			=> 10,
        'transport'   		=> 'postMessage',
        'js_vars'     		=> array(
            array(
                'element'  		=> '.knowmore-btn',
                'function' 		=> 'html',
            ),
        ),				
	) );
	
	// button 1 url
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'     			=> 'text',
		'settings' 			=> 'rexly_slide_button_1_url',
		'label'    			=> __( 'Button 1 url', 'rexly' ),
		'section'  			=> 'rexly_header_section',
		'sanitize_callback' => 'esc_url_raw',
		'default'  			=> '#',
		'priority' 			=> 10,				
	) );
	
	// button 1 target
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'     			=> 'checkbox',
		'settings' 			=> 'rexly_slide_button_1_target',
		'label'    			=> __( 'open button 1 url in a new window', 'rexly' ),
		'section'  			=> 'rexly_header_section',
		'priority' 			=> 10,
		'default'     		=> '0',		
	) );
	
	// button 2 text
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'     			=> 'text',
		'settings' 			=> 'rexly_slide_button_2_text',
		'label'    			=> __( 'Button 2 text', 'rexly' ),
		'section'  			=> 'rexly_header_section',
		'default'  			=> __( 'Download Rexly', 'rexly' ),
		'priority' 			=> 10,
        'transport'   		=> 'postMessage',
        'js_vars'     		=> array(
            array(
                'element'  		=> '.download-btn',
                'function' 		=> 'html',
            ),
        ),				
	) );
	
	// button 2 url
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'     			=> 'text',
		'settings' 			=> 'rexly_slide_button_2_url',
		'label'    			=> __( 'Button 2 url', 'rexly' ),
		'section'  			=> 'rexly_header_section',
		'sanitize_callback' => 'esc_url_raw',
		'default'  			=> '#',
		'priority' 			=> 10,				
	) );
	
	// button 2 target
	rexly_Kirki::add_field( 'rexly_config', array(
		'type'     			=> 'checkbox',
		'settings' 			=> 'rexly_slide_button_2_target',
		'label'    			=> __( 'open button 2 url in a new window', 'rexly' ),
		'section'  			=> 'rexly_header_section',
		'priority' 			=> 10,
		'default'     		=> '0',		
	) );
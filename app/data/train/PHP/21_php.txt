<?php
/*
Template Name: Full Page Width Template
*/

get_header();

  while ( have_posts() ) { 
    the_post();
    get_template_part( 'content', 'page-full' );

   } // end of the loop

get_footer();
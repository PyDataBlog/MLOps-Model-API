<?php
/**
 * The template for displaying all pages.
 *
 * This is the template that displays all pages by default.
 * Please note that this is the WordPress construct of pages
 * and that other 'pages' on your WordPress site will use a
 * different template.
 *
 * @package WordPress
 * @subpackage Twenty_Twelve
 * @since Twenty Twelve 1.0
 */

get_header(); ?>
<div class="content-wrapper">
 <?php while ( have_posts() ) : the_post(); ?>
				<?php get_template_part( 'content', 'page-blog' ); ?>
				<?php //comments_template( '', true ); ?>
			<?php endwhile; // end of the loop. ?>
  
<div class="content-saparator-line"> <img src="<?php echo get_template_directory_uri(); ?>/images/saprator-circle.jpg" width="27" height="13"> </div>

<?php //get_sidebar(); ?> 
 </div>
</div>
			

	

<?php get_footer(); ?>
<?php
/*
 * Template Name: Blog
 */
get_header(); ?>
<?php while ( have_posts() ) : the_post(); ?>
<div id="blog">
<?php get_template_part( 'content', 'introtext' ); ?>
	<section class="features">
    	<div class="container" role="main">
        	<div class="row">
            	<div class="span9">           
              		<?php get_template_part( 'content', get_post_format() ); ?>
				</div>
                <div class="span3 sidebar">
                	<div class="blognav">
                    	<?php get_sidebar(); ?>
   					</div>
                </div>
        	</div>
        </div>
	</section>
</div>    
<?php endwhile; // end of the loop. ?>
<?php get_footer(); ?>
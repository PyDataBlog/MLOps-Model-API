<?php
/**
 * The Template for displaying all single posts.
 *
 * @package WordPress
 * @subpackage Twenty_Twelve
 * @since Twenty Twelve 1.0
 */

get_header(); ?>
<div class="content-wrapper">
 
    
			<?php while ( have_posts() ) : the_post(); ?>

				<?php get_template_part( 'content', 'singleportfolio'); ?>

				
				<?php //comments_template( '', true ); ?>

			<?php endwhile; // end of the loop. ?>
    
      
      
   


		
  <div class="content-saparator-line"> <img src="<?php echo get_template_directory_uri(); ?>/images/saprator-circle.jpg" width="27" height="13"> </div>
<div class="our-clients">
    <h2>Our Clients</h2>
    <div class="client-slider-small-media">
      <button class="next-client"> </button>
      <button class="prev-client"> </button>
    </div>
    <div class="clear"></div>
    <div class="main-client-slider">
      <button class="next-client"> </button>
      <div class="slide-images-2">
        <ul>
  <?php

// The Query
$query = new WP_Query( array('post_type'=>'our_clients', 'order'=>'DESC', 'posts_per_page'=>'-1'));

// The Loop
if ( $query->have_posts() ) {
	while ( $query->have_posts() ) {
		$query->the_post();
	$url = wp_get_attachment_url( get_post_thumbnail_id($post->ID) );

?>		      
          <li><img src="<?php echo $url; ?>" alt="img"></li>
          
  <?php
	
	}
} else {
	// no posts found
}
/* Restore original Post Data */
wp_reset_postdata();
?>                  
         
        </ul>
      </div>
      <button class="prev-client"> </button>
    </div>
  </div>  
</div>
			
 </div> 
<?php //get_sidebar(); ?>
<?php get_footer(); ?>
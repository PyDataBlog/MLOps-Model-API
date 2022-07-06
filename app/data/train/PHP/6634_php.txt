
<?php get_header(); ?>
<div id="main-content"> <!--wraps the stuff in the white - left sidebar, center, right sidebar -->

  <!-- FIX; sidebar is missing from this page -->
    
    <div id="page-article">

	<?php if (have_posts()) : ?>

 			<?php $post = $posts[0]; // Hack. Set $post so that the_date() works. ?>

			<?php /* If this is a category archive */ if (is_category()) { ?>
				<h2 class="pagehead"><?php single_cat_title(); ?></h2> 

			<?php /* If this is a tag archive */ } elseif( is_tag() ) { ?>
				<h2>Posts Tagged &#8216;<?php single_tag_title(); ?>&#8217;</h2>

			<?php /* If this is a daily archive */ } elseif (is_day()) { ?>
				<h2>Archive for <?php the_time('F jS, Y'); ?></h2>

			<?php /* If this is a monthly archive */ } elseif (is_month()) { ?>
				<h2>Archive for <?php the_time('F, Y'); ?></h2>

			<?php /* If this is a yearly archive */ } elseif (is_year()) { ?>
				<h2>Archive for <?php the_time('Y'); ?></h2>

			<?php /* If this is an author archive */ } elseif (is_author()) { ?>
				<h2>Author Archive</h2>

			<?php /* If this is a paged archive */ } elseif (isset($_GET['paged']) && !empty($_GET['paged'])) { ?>
				<h2>Blog Archives</h2>
			
			<?php } ?>

			<?php include (TEMPLATEPATH . '/inc/nav.php' ); ?>

			<?php while (have_posts()) : the_post(); ?>
			
		  <?php if ( function_exists("has_post_thumbnail") && has_post_thumbnail() ) { the_post_thumbnail(array(90,90), array("class" => "thumbnail-cat")); } ?>

                                
                                
			<h2 id="post-<?php the_ID(); ?>"><a href="<?php the_permalink() ?>"><?php the_title(); ?></a></h2>



			<div class="entry">

				<?php the_content("Read more . . ."); ?>

				<!-- <?php wp_link_pages(array('before' => 'Pages: ', 'next_or_number' => 'number')); ?> -->

			</div>


                        <div class="date">
				
                            Posted on: <?php the_time('F jS, Y') ?>
			</div>
                        
                        <?php endwhile; ?>
                        </div>	
    
    			

	<?php else : ?>

		<h2>Page Not Available Yet</h2>
                
                <p>We are in the process of updating all of the content for the new school year. We hope to have current information on this topic very very soon. Please check back or <a href="<?php bloginfo('rss2_url'); ?>">subscribe to our RSS feed</a>.


	<?php endif; ?>
        
        
    </div> <!-- END center-wrap -->

    	


<?php get_footer(); ?>
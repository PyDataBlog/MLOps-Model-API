<?php
/**
 * The main template file.
 *
 * This is the most generic template file in a WordPress theme
 * and one of the two required files for a theme (the other being style.css).
 * It is used to display a page when nothing more specific matches a query.
 * E.g., it puts together the home page when no home.php file exists.
 * Learn more: http://codex.wordpress.org/Template_Hierarchy
 *
 * @package morrisseys_catering
 */

get_header(); ?>
		<?php if ( have_posts() ) : ?>

			<?php /* Start the Loop */ ?>
			<?php while ( have_posts() ) : the_post(); ?>

				<?php
					/* Include the Post-Format-specific template for the content.
					 * If you want to override this in a child theme, then include a file
					 * called content-___.php (where ___ is the Post Format name) and that will be used instead.
					 */
					get_template_part( 'content', get_post_format() );
				?> 
			<?php endwhile; ?>

			<?php morrisseys_catering_content_nav( 'nav-below' ); ?>

		<?php else : ?>

			<?php get_template_part( 'content', 'none' ); ?>

		<?php endif; ?> 
<div id="food-content">
	<h1>image</h1>
	<div id="inner-box"> 
		<p>Fresh. Local. Family Owned.</p>
			<a class="button" href="#"> 
				<h1>Icon menu </h1> 	
				<p>Browse Menus</p>  			
			</a>
	</div>  
</div>


<div class="wrapper">
	<blockquote id="feedback"> 
		<p> Thank you for providing us each delicious food and service for the St. Mary's
			50th Anniversary Celebration! The food was excellent and you are all the best to work with! We are grateful for you, thank you.<br> 
				<span>-Siobhan </span>
		</p>
		<div class="splitter"> 
		</div> 
		<br>
		<br> 
		<p>Many thanks for your kindness during such a sad time for us - you were so helpful. Everyone enjoyed your wonderful food at the church reception. <br> <span>-Mrs. Showalter</span></p> 
	</blockquote>

	<div id="contact-info">
		<div id="contact-box"> 
			<img src="wp-content/themes/morrisseys_catering/images/icon_phone.png" alt="phone.icon"> <p> (804) 592-2188</p> 
		</div> 
		<br>
		<br>
		<div class="address-info"> 
			<p> 8901 Three Chopt Road, Suite A, Richmond, Va 23229 </p> 
			<p> Located in the Westbury Pharmacy Center</p>
		</div> <!--address-info -->
	</div>  <!-- contact-info" --> 
</div> 
<?php get_sidebar(); ?>
<?php get_footer(); ?>

<?php 
/*
Template Name: Wide Header Image Page
*/
?>

<?php if ( have_posts() ) : while ( have_posts() ) : the_post(); ?>


<?php if(get_the_post_thumbnail()): ?>

<div class="wrapper-negative container-fluid wide-header">
	<div class="row">
		<div class="col-sm-12">
			<?php the_post_thumbnail(full); ?>
		</div>
	</div>
</div>

<?php endif; ?>	


<?php get_template_part('templates/page', 'header'); ?>

<div class="container wrapper">
	<div class="row">
		<div class="col-sm-12">
			<?php the_content(); ?>
		</div>
	</div>
</div>


<?php endwhile; ?>
<?php endif; ?> 
<?php
//Names the page template for each section
/*
Template Name: Early College
*/
get_header(); ?>
<?php get_template_part( 'parts/banners' ); ?>

<div class="row">
<div class="container">
	
<?php // Gets the alert custom post type id for each sub page needing special announcement
$post_id = 5447;
$queried_post = get_post($post_id);
			$content = $queried_post->post_content;
			$content = apply_filters('the_content', $content);
			if ($content) { ?>
<div class='alert-box alert'>
<?php echo "$content"; ?>
<a href='#' class='close' aria-hidden="true" role="button"><span class='icon-remove-circle'></span></a></div>
<?php
}
else {
}
?>


<?php get_template_part( 'parts/content' ); ?>

<?php get_template_part( 'parts/sidebars/early-college-sidebar' ); ?>

</div>
</div>
<?php get_footer(); ?>
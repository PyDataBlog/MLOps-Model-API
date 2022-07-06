<?php
/**
 * @package WordPress
 * @subpackage ThemeWoot
 * @author ThemeWoot Team 
 *
 * This source file is subject to the GNU GENERAL PUBLIC LICENSE (GPL 3.0)
 * that is bundled with this package in the file LICENSE.txt.
 * It is also available through the world-wide-web at this URL:
 * http://www.gnu.org/licenses/gpl-3.0.txt
 */
$link = twoot_get_frontend_func('meta', 'post_link')=='custom' && twoot_get_frontend_func('meta', 'custom_url')!=false? twoot_get_frontend_func('meta', 'custom_url'):get_permalink();
?>
<?php if(has_post_thumbnail()) : ?>
<figure class="featured-image img-preload img-hover">
	<a href="<?php the_permalink(); ?>" title="<?php the_title_attribute(); ?>">
	<?php echo twoot_get_frontend_func('resize_thumbnail', get_post_thumbnail_id(), get_the_title(), 580, 430, true); ?>
	<div class="overlay"></div>
	<header class="item-head">
		<h3 class="title item-title"><a href="<?php echo $link; ?>" title="<?php the_title_attribute(); ?>"><?php the_title(); ?></a></h3>
		<?php echo get_the_term_list( get_the_ID(), 'portfolio_cat', '<div class="cats meta">', ', ', '</div>' ); ?>
	</header>
	</a>
</figure>
<?php endif; ?>
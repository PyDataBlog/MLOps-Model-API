<?php
/**
 * The loop that displays a single post.
 *
 * The loop displays the posts and the post content.  See
 * http://codex.wordpress.org/The_Loop to understand it and
 * http://codex.wordpress.org/Template_Tags to understand
 * the tags used in it.
 *
 * This can be overridden in child themes with loop-single.php.
 *
 * @package WordPress
 * @subpackage Twenty_Ten
 * @since Twenty Ten 1.2
 */
?>

<?php if ( have_posts() ) while ( have_posts() ) : the_post(); ?>
<div id="loopsinglecontent">

				<div id="post-<?php the_ID(); ?>" <?php post_class(); ?>>
					<h2><a HREF="javascript:javascript:history.go(-1)"><< Back</a>
					<h3><?php the_date(); ?></h3>
					<h1 class="entry-title"><?php the_title(); ?></h1>
					<div class="entry-content">
						<?php the_content();?>
				<div id="entry-author-info">
				Posted by <b><?php the_author(); ?></b>
					</div>						
					<?php wp_link_pages( array( 'before' => '<div class="page-link">' . __( 'Pages:', 'twentyten' ), 'after' => '</div>' ) ); ?>
					</div><!-- .entry-content -->


<?php endwhile; // end of the loop. ?>
</div>
</div>
<div id="articlesidebar">

<h2>Recent Articles</h2>
<ul>
<?php query_posts( array(
      'posts_per_page' => 10,
      'category_name' => Blog,
      'paged' => ( get_query_var('paged') ? get_query_var('paged') : 1 ),
 ));
?>
			<?php if ( have_posts() ) while ( have_posts() ) : the_post(); ?>
<a href="<?php the_permalink() ?>" rel="bookmark">
<li><h1><?php the_date(); ?></h1>
<?php the_title(); ?>
</li></a>
<?php endwhile; ?>
</ul>
<?php get_sidebar();?>

</div>


<?php get_footer(); ?>
 <STYLE type="text/css">
   #primary {display:inherit !important;}
   #article{ margin-bottom:10px !important;
   }
 </STYLE>
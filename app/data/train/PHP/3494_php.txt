<?php
/**
 *	The template for displaying 404.
 *
 *	@package 9Pixels.
 */
get_header();
?>
<div class="wrap">
	<div id="custom-page">
		<div class="fullwidth-page-title">
			<?php
			if ( get_theme_mod( 'denta_lite_404_general_title', 'Error' ) ) {
				echo '<h3>'. esc_attr( get_theme_mod( 'denta_lite_404_general_title', 'Error' ) ) .'</h3>';
			}
			?>
		</div><!--/.fullwidth-page-title-->
		<div class="custom-page-content">
			<?php
			if ( get_theme_mod( 'denta_lite_404_general_subtitle', 'The page you were looking for was not found.' ) ) {
				echo '<h2>'. esc_attr( get_theme_mod( 'denta_lite_404_general_subtitle', 'The page you were looking for was not found.' ) ) .'</h2>';
			}
			?>
			<div class="page-content-entry">
				<?php
				if ( get_theme_mod( 'denta_lite_404_general_entry', 'The page you are looking for does not exist, I can take you to the <a href="'. esc_url( home_url() ) .'" title="'. __( 'home page', 'denta-lite' ) .'">home page</a>.' ) ) {
					echo get_theme_mod( 'denta_lite_404_general_entry', 'The page you are looking for does not exist, I can take you to the <a href="'. esc_url( home_url() ) .'" title="'. __( 'home page', 'denta-lite' ) .'">home page</a>.' );
				}
				?>
			</div><!--/.page-content-entry-->
		</div><!--/.custom-page-content-->

		<?php
		if ( get_theme_mod( 'denta_lite_404_contact_title', 'Contact' ) || get_theme_mod( 'denta_lite_404_contact_email', 'contact@yourwebsite.com' ) || get_theme_mod( 'denta_lite_404_contact_telephone', '0001234567' ) || get_theme_mod( 'denta_lite_404_contact_url', 'http://www.yourwebsite.com' ) ) { ?>

			<div class="custom-page-info">
				<?php
				if ( get_theme_mod( 'denta_lite_404_contact_title', 'Contact' ) ) {
					echo '<h2>'. esc_attr( get_theme_mod( 'denta_lite_404_contact_title', 'Contact' ) ) .'</h2>';
				}

				if ( get_theme_mod( 'denta_lite_404_contact_email', 'contact@yourwebsite.com' ) || get_theme_mod( 'denta_lite_404_contact_telephone', '0001234567' ) || get_theme_mod( 'denta_lite_404_contact_url', 'http://www.yourwebsite.com' ) ) { ?>

					<ul>
						<?php
						if ( get_theme_mod( 'denta_lite_404_contact_email', 'contact@yourwebsite.com' ) ) {
							echo '<li><i class="fa fa-envelope"></i><a href="mailto:'. get_theme_mod( 'denta_lite_404_contact_email', 'contact@yourwebsite.com' ) .'" title="'. get_theme_mod( 'denta_lite_404_contact_email', 'contact@yourwebsite.com' ) .'">'. get_theme_mod( 'denta_lite_404_contact_email', 'contact@yourwebsite.com' ) .'</a></li>';
						}

						if ( get_theme_mod( 'denta_lite_404_contact_telephone', '0001234567' ) ) {
							echo '<li><i class="fa fa-phone"></i><a href="tel:'. esc_attr( get_theme_mod( 'denta_lite_404_contact_telephone', '0001234567' ) ) .'" title="'. esc_attr( get_theme_mod( 'denta_lite_404_contact_telephone', '0001234567' ) ) .'">'. esc_attr( get_theme_mod( 'denta_lite_404_contact_telephone', '0001234567' ) ) .'</a></li>';
						}

						if ( get_theme_mod( 'denta_lite_404_contact_url', 'http://www.yourwebsite.com' ) ) {
							echo '<li><i class="fa fa-home"></i><a href="'. esc_url( get_theme_mod( 'denta_lite_404_contact_url', 'http://www.yourwebsite.com' ) ) .'" title="'. esc_url(  get_theme_mod( 'denta_lite_404_contact_url', 'http://www.yourwebsite.com' ) ) .'">'. esc_url( get_theme_mod( 'denta_lite_404_contact_url', 'http://www.yourwebsite.com' ) ) .'</a></li>';
						}
						?>
					</ul>

				<?php }
				?>
			</div><!--/.custom-page-info-->

		<?php }
		?>

	</div><!--/#custom-page-->
</div><!--/.wrap-->
<?php get_footer(); ?>
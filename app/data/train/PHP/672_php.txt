<?php
/**
 * Template Name: Library
 * @package mjv-theme
 */
if (is_home()) :
    get_header();
else :
    get_header('insiders');
endif;
?>

<div id="primary" class="content-area library">
    <main id="main" class="site-main" role="main">
        <?php
            //carrega os cases, clients e content
            get_template_part('template-parts/content', 'library');
        ?>

    </main><!-- #main -->
</div><!-- #primary -->

<?php
get_sidebar();
get_footer();

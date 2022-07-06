<?php
/*
*  Template Name: Archive South Florida Condo 
*/
get_header('frontpage');

// get_header();
?>

    <!-- Page Head -->
    <?php get_template_part("banners/default_page_banner"); ?>
<!-- <h1>Archive S Fl condos</h1> -->
    <!-- Content -->
    <div class="container contents single">


    <!--=============================================
    =            Moose Breadcrumbs Block            =
    ==============================================-->

    <section class="bread-search-block row-fluid">

        <article class="span8 text-center">

            <div class="breadcrumbs pull-left" typeof="BreadcrumbList" vocab="http://schema.org/">
                <?php if(function_exists('bcn_display'))
                {
                    //bcn_display();
                }?>
            </div>
            
        </article>
        <article class="span4">

            <?php //echo get_search_form(); ?>

            <?php //echo do_shortcode('[autocomplete-post-search]'); ?>
            
        </article>


    </section>    


        <div class="row">
            <?php //get_sidebar('pages'); ?>

            <div class="span12 main-wrap">
                <!-- Main Content -->
                <div class="main">

                    <div class="inner-wrapper">
                    <h1 class="page-title text-center">South Florida Condos</h1>

                    <!--========================================
                    =            This is Moose Loop            =
                    =========================================-->
                    
                    <?php 

                        $args = array(
                            'post_type' => 'south-florida-condos',
                            'posts_per_page' => -1,
                            'post_status' => 'publish',
                            'order' => 'ASC',
                            'post_parent' => 0
                        );
                        $the_query = new WP_Query( $args ); 

                        // print_r( $the_query );

                    ?>

                
                    <article id="post-<?php the_ID(); ?>" <?php post_class("clearfix"); ?>>


                            <?php 
                                // The Loop

                                if ( $the_query->have_posts() ) :

                                    while ( $the_query->have_posts() ) : $the_query->the_post(); ?>

                                    <!-- <h3 class="post-title"><?php the_title(); ?></h3> -->
                                    <div class="row-fluid main-loop">


                                    <?php //echo the_ID(); ?>
                                    <?php 

                                      $condo_post_link_1 = get_field('condo_post_link_1');
                                      $condo_post_link_1_text = get_field('condo_post_link_1_text');
                                      // echo $condo_post_link_1;

                                      $condo_post_link_2 = get_field('condo_post_link_2');
                                      $condo_post_link_2_text = get_field('condo_post_link_2_text');
                                      // echo $condo_post_link_2;

                                      $condo_post_link_3 = get_field('condo_post_link_3');
                                      $condo_post_link_3_text = get_field('condo_post_link_3_text');
                                      // echo $condo_post_link_3;  


                                    ?>

                                        <div class="span5">
                                                
                                            <?php if ( has_post_thumbnail() ) : ?> 
                                                
                                                <div class="featured-img">
                                                    <a href="<?php the_permalink(); ?>" title=""><?php the_post_thumbnail( 'custom-post-index', array('class' => 'img-responsive'));  ?></a>

                                                </div>
                                            <?php else : ?> 
                                               
                                                    <a href="<?php the_permalink(); ?>" title=""><img class="img-responsive" src="/wp-content/uploads/2016/08/mical-default-img.jpg" alt=""></a>


                                            <?php endif; ?>
                                        
                                        </div> <!-- SPAN 5 -->

                                        <div class="span7">

                                            <h3 class="post-title"><a href="<?php the_permalink(); ?>"><?php the_title(); ?></a></h3>
                                            <hr>                              
                                            <?php the_excerpt(); ?>     
                                            <a class="btn btn-info pull-right" href="<?php the_permalink(); ?>" style="text-decoration: none;">SEE ALL CONDOS</a>
                                            
                                            <div class="link-block row-fluid text-center">

                                                <article class="span4">     
                                                    <a class="" href="<?php echo $condo_post_link_1; ?>">
                                                        <!-- Lauderdale By The Sea Condos -->
                                                        <?php echo $condo_post_link_1_text; ?>
                                                    </a>
                                                </article>

                                                <article class="span4">
                                                    <a class="" href="<?php echo $condo_post_link_2; ?>">
                                                        <!-- Bay Harbor Islands Condos -->
                                                        <?php echo $condo_post_link_2_text; ?>

                                                    </a>

                                                </article>
                                                <article class="span4">
                                                    <a class="" href="<?php echo $condo_post_link_3; ?>">
                                                        <!-- North Miami Beach Condos -->
                                                        <?php echo $condo_post_link_3_text; ?>

                                                    </a>
                                                </article>

                                            </div> <!-- LINK BLOCK -->



                                        </div>  <!-- SPAN 7 -->

                                    </div>  <!-- END ROW FLUID -->      
                                        
                                <?php 
                                    endwhile;

                                endif;

                                // Reset Post Data

                                wp_reset_postdata();

                            ?>


                    </article>


                    </div>

                </div><!-- End Main Content -->

            </div> <!-- End span12 -->

        </div><!-- End contents row -->

    </div><!-- End Content -->

<?php get_footer(); ?>
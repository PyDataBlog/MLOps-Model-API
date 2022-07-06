<?php while (have_posts()) : the_post(); ?>
  <article <?php post_class(); ?>>

    <div class="entry-content content-medicina-prepagada">
    <header class="menu-med-prep text-center">

<ul class="list-inline">
    <?php $loop = new WP_Query( array( 'post_type' => 'aps', 'order' => 'ASC', 'category_name'=> 'link'));?>

      <?php while ( $loop->have_posts() ) : $loop->the_post(); 
  
      $slug = get_post_field( 'post_name', get_post() );
        ?>
  <li class="boton-aps"><a href="<?php the_permalink();?>"><span class="aps-title-menu text-uppercase texto<?php echo $slug; ?>"><?php the_title(); ?></span></a></li>
  <?php endwhile; wp_reset_query(); ?>
</ul>

    </header>
<?php $slug = get_post_field( 'post_name', get_post() ); ?>
    <div class="row">
      <div class="col-sm-12">
        <div class="title-big-aps">
          <div class="row">
            <div class="col-sm-6">
              <div class="aps-text-big">
                <h2><span class="titulo-plan-big titulo-aps-big">Plan </span><span class="text-uppercase titulo-plan titulo-aps fondo<?php echo $slug; ?>"><?php the_title(); ?></span></h2>
              </div>
            </div>
            <div class="col-sm-6">
              <img src="<?php the_post_thumbnail_url('full'); ?>" class="img-responsive">
            </div>
          </div>
        </div>
        <div class="wrapper-aps"><?php the_content(); ?></div>


        <div class="wrapper-botones">
          
          <?php $id_empresarial=get_category_by_slug( 'empresarial' ); 
          
          $category=get_the_category();
          if ($category[0]->name === 'empresarial') {

            if( get_adjacent_post(true, '', true, 'category') ) { 
              $post_previo=get_adjacent_post(true, '', true, 'category');
                if((in_category('empresarial', $post_previo))){
                  $post_previo_slug=get_post_field( 'post_name', $post_previo );
                  $post_previo_slug=str_replace("-", " ", $post_previo_slug);


                  echo '<div class="texto-azul pull-left boton-nav">';
                  previous_post_link('%link', '<span class="glyphicon glyphicon-chevron-left"></span>'.$post_previo_slug);
                  echo "</div>";
                } 
                else { 
                  $first = new WP_Query( array( 'post_type' => 'aps', 'order' => 'DESC', 'category__in'=> $id_empresarial->term_id)); $first->the_post();
                  $post_previo_slug=get_post_field( 'post_name' );
                  $post_previo_slug=str_replace("-", " ", $post_previo_slug);

                  echo '<div class="texto-azul pull-left boton-nav"><a href="' . get_permalink() . '"><span class="glyphicon glyphicon-chevron-left"></span>'.$post_previo_slug.'</a></div>';
                  wp_reset_query();
                };
            }; 
    
            if( get_adjacent_post(true, '', false, 'category') ) { 
              $post_sig=get_adjacent_post(true, '', false, 'category');
              $post_sig_slug=get_post_field( 'post_name', $post_sig );

              echo '<div class="texto-azul pull-right boton-nav">';
              next_post_link('%link', $post_sig_slug.'<span class="glyphicon glyphicon-chevron-right"></span></div>');
            } 
            else { 
              $last = new WP_Query( array( 'post_type' => 'aps', 'order' => 'ASC', 'category__in'=> $id_empresarial->term_id)); $last->the_post();
              $post_sig_slug=get_post_field( 'post_name' );
              $post_sig_slug=str_replace("-", " ", $post_sig_slug);
              // $post_sig_slug=get_post_field( 'post_name', $post_sig );
              echo '<div class="texto-azul pull-right boton-nav"><a href="' . get_permalink() . '"><span class="textobotonnav">'.$post_sig_slug.'</span><span class="glyphicon glyphicon-chevron-right"></span></a></div>';
              wp_reset_query();
            };
          }
          else{?>
        <div class="wrapper-botones text-center">
          <a href="#" class="boton-med-prep">Cotización Familiar</a>
          <a href="#" class="boton-med-prep">Cotizar Empresa</a>
        </div>
            <?php }?>
          </div>
        </div>  
      </div> 
    </div>

    </div>
  </article>
<?php endwhile; ?>

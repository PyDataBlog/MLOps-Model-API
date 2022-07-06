<div class="entry-content"><?php the_content(); ?></div>

<div class="container">
  <?php if( have_rows( 'projects' ) ): ?>
    <div class="row projects">
      <?php $timing = 0; ?>
      <?php while( have_rows ( 'projects' ) ) : the_row(); ?>
        <div class="col-sm-12 col-md-6 col-lg-4">
          <a data-sr="enter bottom, move 150px, wait <?php echo $timing;?>s" class="card project-card"  href="<?php echo the_sub_field( 'link' ); ?>" style="background-image: url(<?php echo the_sub_field('image'); ?>)">
            <h1><?php the_sub_field('title'); ?></h1>
            <p><?php the_sub_field('description'); ?></p>
            <span>Discover</span>
          </a>
        </div>
        <?php $timing+=0.1; ?>
      <?php endwhile; ?>
    </div>
  <?php elseif( have_rows( 'charities' ) ) : ?>
    <div class="row projects">
      <?php while( have_rows ( 'charities' ) ) : the_row(); ?>
        <div class="col-sm-12 col-md-6 col-lg-4">
          <a class="card project-card" href="<?php echo the_sub_field( 'link' ); ?>" style="background-image: url(<?php echo the_sub_field('image'); ?>)">
            <h1><?php the_sub_field('name'); ?></h1>
            <p><?php the_sub_field('description'); ?></p>
            <span>Learn More</span>
          </a>
        </div>
      <?php endwhile; ?>
    </div>
  <?php endif; ?>
</div>

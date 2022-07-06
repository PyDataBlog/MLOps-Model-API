<?php
/*
Template Name: Signin Template
*/
?>

<?php get_header(); ?>

<div class="container">
<div id="content" class="clearfix row">

<div id="main" class="col-md-12 clearfix" role="main">

<?php if ( function_exists('custom_breadcrumb') ) { custom_breadcrumb(); } ?>

<section class="page-content entry-content clearfix" itemprop="articleBody">
<div class="formheight">
<form class="form-signin">
        <h2 class="form-signin-heading">Please sign in</h2>
        <label for="inputEmail" class="sr-only">Email address</label>
        <input type="email" id="inputEmail" class="form-control" placeholder="Email address" required autofocus>
        <label for="inputPassword" class="sr-only">Password</label>
        <input type="password" id="inputPassword" class="form-control" placeholder="Password" required>
        <div class="checkbox">
          <label>
            <input type="checkbox" value="remember-me"> Remember me
          </label>
        </div>
        <button class="btn btn-lg btn-primary btn-block" type="submit">Sign in</button>
      </form>
    </div>
    </section>
</div>
</div>
</div>

<?php get_footer(); ?>

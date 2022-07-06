<?php session_start();?>
<!DOCTYPE html>
<html lang="pt-br">
<head>
	<meta charset="UTF-8">
	<meta name="viewport" content="width=device-width, initial-scale=1">
		<!-- CSS -->
		<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" integrity="sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7" crossorigin="anonymous">
		<link href="http://maxcdn.bootstrapcdn.com/font-awesome/4.2.0/css/font-awesome.min.css" rel="stylesheet">
		<style type="text/css" media="screen">
			@import url( <?php bloginfo('stylesheet_url'); ?> );
		</style>
		<link href="<?php bloginfo('template_directory'); ?>/css/jquery.bxslider.css" rel="stylesheet" />
		<!-- ICON -->
		<link rel="icon" href="<?php bloginfo('template_directory'); ?>/images/favicon.ico" type="image/x-icon">
		<!-- FONTS -->
		<link href='http://fonts.googleapis.com/css?family=Rubik+One' rel='stylesheet' type='text/css'>
	<title>
			<?php if (is_home()){
			bloginfo('name');
		}elseif (is_category()){
			single_cat_title(); echo ' -  ' ; bloginfo('name');
		}elseif (is_single()){
			single_post_title();
		}elseif (is_page()){
			bloginfo('name'); echo ': '; single_post_title();
		}else {
			wp_title('',true);
		} ?>
	</title>
	<?php wp_head(); ?>
</head>
<body>
	<div class="cart-list">
		<div class="container">
			<?php if (isset($_SESSION['cart'])): ?>
				<p class="col-md-6">Seja bem vindo</p>
				<div class="pull-right">
					<p>Acesse sua <a href="<?php echo get_settings('home'); ?>/pedidos">lista de pedidos <span class="quant-cart glyphicon glyphicon-shopping-cart"></span></a></p>
				</div>
			<?php else: ?>
				<?php $_SESSION['cart']=array(); // Declaring session array?>
			<?php endif ?>
		</div>
	</div>
	<div id="menu-celular">
		<span class='glyphicon glyphicon-menu-hamburger'></span>
	</div>
	<header>
		<div class="container">
				<span class="col-md-4 col-md-offset-4">
					<img src="<?php bloginfo('template_directory'); ?>/images/logoHeader.png">
				</span>
				<ul class="header-menu">
					<li><a href="<?php echo get_settings('home'); ?>">Home</a></li>
					<li id="menu-quem-somos"><a href="<?php echo get_settings('home'); ?>/#quem-somos">Quem somos</a></li>
					<?php $id_da_categoria = get_cat_id('produtos'); $link_da_categoria = get_category_link($id_da_categoria); ?>
					<li id="menu-nossos-produtos"><a href="<?php echo $link_da_categoria;?>">Produtos</a></li>
					<li id="menu-fale-conosco"><a href="<?php echo get_settings('home'); ?>/#fale-conosco">Fale conosco</a></li>
				</ul>
			</div>
			<!-- SEARCH -->
			<div class="col-md-6 search-header">
			<form role="search" method="get" id="searchform" action="<?php echo get_option('home'); ?>">
				<div class="input-group">
					<input value="" name="s" id="s" type="text" class="form-control" placeholder="Procurando por...">
					<span class="input-group-btn">
					<button id="searchsubmit" value="" type="submit" class="btn btn-default" type="button"><i class="glyphicon glyphicon-search"></i></button>
				      </span>
				</div>
				</form>
			</div>
		</div>
	</header>
	<!--FIM DO HEADER -->

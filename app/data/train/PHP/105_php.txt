<!DOCTYPE html>
<html lang="en">
<head> 
	<meta charset="utf-8"> 
	<meta name="viewport" content="width=device-width, initial-scale=1.0"> 
	<meta name="description" content="Creative One Page Parallax Template">
	<meta name="keywords" content="Creative, Onepage, Parallax, HTML5, Bootstrap, Popular, custom, personal, portfolio" /> 
	<meta name="author" content=""> 
	<title>Sistem Monitoring KP - Sisfor ITS</title> 
	<link href="<?php echo base_url().'/asset_umum/css/bootstrap.min.css';?>" rel="stylesheet">
	<link href="<?php echo base_url().'/asset_umum/css/prettyPhoto.css';?>" rel="stylesheet"> 
	<link href="<?php echo base_url().'/asset_umum/css/font-awesome.min.css';?>" rel="stylesheet"> 
	<link href="<?php echo base_url().'/asset_umum/css/animate.css';?>" rel="stylesheet"> 
	<link href="<?php echo base_url().'/asset_umum/css/main.css';?>" rel="stylesheet">
	<link href="<?php echo base_url().'/asset_umum/css/responsive.css';?>" rel="stylesheet"> 
	<!--[if lt IE 9]> <script src="js/html5shiv.js"></script> 
	<script src="js/respond.min.js"></script> <![endif]--> 
	<link rel="shortcut icon" href="images/ico/favicon.png"> 
	<link rel="apple-touch-icon-precomposed" sizes="144x144" href="<?php echo base_url().'/asset_umum/images/ico/apple-touch-icon-144-precomposed.png';?>"> 
	<link rel="apple-touch-icon-precomposed" sizes="114x114" href="<?php echo base_url().'/asset_umum/images/ico/apple-touch-icon-114-precomposed.png';?>"> 
	<link rel="apple-touch-icon-precomposed" sizes="72x72" href="<?php echo base_url().'/asset_umum/images/ico/apple-touch-icon-72-precomposed.png';?>"> 
	<link rel="apple-touch-icon-precomposed" href="<?php echo base_url().'/asset_umum/images/ico/apple-touch-icon-57-precomposed.png';?>">
</head><!--/head-->
<body>
	<div class="preloader">
		<div class="preloder-wrap">
			<div class="preloder-inner"> 
				<div class="ball"></div> 
				<div class="ball"></div> 
				<div class="ball"></div> 
				<div class="ball"></div> 
				<div class="ball"></div> 
				<div class="ball"></div> 
				<div class="ball"></div>
			</div>
		</div>
	</div><!--/.preloader-->
	<header id="navigation"> 
		<div class="navbar navbar-inverse navbar-fixed-top" role="banner"> 
			<div class="container"> 
				<div class="navbar-header"> 
					<button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".navbar-collapse"> 
						<span class="sr-only">Toggle navigation</span> <span class="icon-bar"></span> <span class="icon-bar"></span> <span class="icon-bar"></span> 
					</button> 
					<a class="navbar-brand" href="#navigation"><h1><img src="<?php echo base_url().'/asset_umum/images/logo1.png';?>" alt="logo"></h1></a> 
				</div> 
				<div class="collapse navbar-collapse"> 
					<ul class="nav navbar-nav navbar-right"> 
						<li class="scroll active"><a href="#navigation">Home</a></li> 
						<li class="scroll"><a href="<?php echo base_url('index.php/c_user/lapor');?>">Laporan</a></li>
						<li class="scroll"><a href="#history">History</a></li> 
						<li class="scroll"><a href="#contact">Contact</a></li>
						<li class="scroll"><a href="<?php echo base_url('index.php/c_user/logout');?>">Logout</a></li>						
					</ul> 
				</div> 
			</div> 
		</div><!--/navbar--> 
	</header> <!--/#navigation--> 

	<section id="home">
		<div class="home-pattern"></div>
		<div id="main-carousel" class="carousel slide" data-ride="carousel"> 
			<ol class="carousel-indicators">
				<li data-target="#main-carousel" data-slide-to="0" class="active"></li>
				<li data-target="#main-carousel" data-slide-to="1"></li>
				<li data-target="#main-carousel" data-slide-to="2"></li>
			</ol><!--/.carousel-indicators--> 
			<div class="carousel-inner">
				<div class="item active" style="background-image: url(<?php echo base_url().'/asset_umum/images/slider/s1.jpg';?>)"> 
					<div class="carousel-caption"> 
						<div> 
							<h2 class="heading animated bounceInDown">Institut Teknologi Sepuluh Nopember</h2> 
							<p class="animated bounceInUp">Information Systems Department</p>
						</div> 
					</div> 
				</div>
				<div class="item" style="background-image: url(<?php echo base_url().'asset_umum/images/slider/s2.jpg';?>)"> 
					<div class="carousel-caption"> <div> 
						<h2 class="heading animated bounceInDown"><span>Sistem Monitoring KP</span></h2> 
						<p class="animated bounceInUp">Sistem terintegrasi untuk mahasiswa Sistem Informasi</p>
					</div> 
				</div> 
			</div> 
			<div class="item" style="background-image: url(<?php echo base_url().'asset_umum/images/slider/s3.jpg';?>)"> 
				<div class="carousel-caption"> 
					<div> 
						<h2 class="heading animated bounceInRight">Create and Submit!</h2> 
						<p class="animated bounceInLeft">Mudah dan praktis digunakan</p> 
						<a class="btn btn-default slider-btn animated bounceInUp" href="<?php echo base_url('index.php/c_user/logout');?>">Get Started</a> 
					</div> 
				</div> 
			</div>
		</div><!--/.carousel-inner-->

		<a class="carousel-left member-carousel-control hidden-xs" href="#main-carousel" data-slide="prev"><i class="fa fa-angle-left"></i></a>
		<a class="carousel-right member-carousel-control hidden-xs" href="#main-carousel" data-slide="next"><i class="fa fa-angle-right"></i></a>
	</div> 

</section><!--/#home-->

<section id="history"> 
						<div class="container">
							<div class="row text-center clearfix">
								<div class="col-sm-8 col-sm-offset-2">
									<h2 class="title-one">History</h2>
									<p class="blog-heading">Historis pengumpulan laporan KP</p>
								</div>
							</div> 
							<div class="row">
								<div class="col-sm-4">
									<div class="single-blog">
										<img src="images/blog/1.jpg" alt="" />
										<h2>Lorem ipsum dolor sit amet</h2>
										<ul class="post-meta">
											<li><i class="fa fa-pencil-square-o"></i><strong> Posted By:</strong> John</li>
											<li><i class="fa fa-clock-o"></i><strong> Posted On:</strong> Apr 15 2014</li>
										</ul>
										<div class="blog-content">
											<p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.</p>
										</div>
										<a href="" class="btn btn-primary" data-toggle="modal" data-target="#blog-detail">Read More</a>
									</div>
									<div class="modal fade" id="blog-detail" tabindex="-1" role="dialog" aria-hidden="true">
										<div class="modal-dialog">
											<div class="modal-content">
												<div class="modal-body">
													<button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
													<img src="images/blog/3.jpg" alt="" />
													<h2>Lorem ipsum dolor sit amet</h2>
													<p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.</p><p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.</p>
												</div> 
											</div>
										</div>
									</div>
								</div>
								<div class="col-sm-4">
									<div class="single-blog">
										<img src="images/blog/2.jpg" alt="" />
										<h2>Lorem ipsum dolor sit amet</h2>
										<ul class="post-meta">
											<li><i class="fa fa-pencil-square-o"></i><strong> Posted By:</strong> John</li>
											<li><i class="fa fa-clock-o"></i><strong> Posted On:</strong> Apr 15 2014</li>
										</ul>
										<div class="blog-content">
											<p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.</p>
										</div>
										<a href="" class="btn btn-primary" data-toggle="modal" data-target="#blog-two">Read More</a>
									</div>
									<div class="modal fade" id="blog-two" tabindex="-1" role="dialog" aria-hidden="true">
										<div class="modal-dialog">
											<div class="modal-content">
												<div class="modal-body">
													<button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
													<img src="images/blog/2.jpg" alt="" />
													<h2>Lorem ipsum dolor sit amet</h2>
													<p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.</p><p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.</p>
												</div>
											</div>
										</div>
									</div>
								</div>
								<div class="col-sm-4">
									<div class="single-blog">
										<img src="images/blog/3.jpg" alt="" />
										<h2>Lorem ipsum dolor sit amet</h2>
										<ul class="post-meta">
											<li><i class="fa fa-pencil-square-o"></i><strong> Posted By:</strong> John</li>
											<li><i class="fa fa-clock-o"></i><strong> Posted On:</strong> Apr 15 2014</li>
										</ul>
										<div class="blog-content">
											<p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.</p>
										</div>
										<a href="" class="btn btn-primary" data-toggle="modal" data-target="#blog-three">Read More</a>
									</div>
									<div class="modal fade" id="blog-three" tabindex="-1" role="dialog" aria-hidden="true">
										<div class="modal-dialog">
											<div class="modal-content">
												<div class="modal-body">
													<button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
													<img src="images/blog/3.jpg" alt="" />
													<h2>Lorem ipsum dolor sit amet</h2>
													<p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.</p><p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.</p>
												</div> 
											</div>
										</div>
									</div>
								</div>
								<div class="col-sm-4">
									<div class="single-blog">
										<img src="images/blog/3.jpg" alt="" />
										<h2>Lorem ipsum dolor sit amet</h2>
										<ul class="post-meta">
											<li><i class="fa fa-pencil-square-o"></i><strong> Posted By:</strong> John</li>
											<li><i class="fa fa-clock-o"></i><strong> Posted On:</strong> Apr 15 2014</li>
										</ul>
										<div class="blog-content">
											<p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.</p>
										</div>
										<a href="" class="btn btn-primary" data-toggle="modal" data-target="#blog-four">Read More</a></div>
										<div class="modal fade" id="blog-four" tabindex="-1" role="dialog" aria-hidden="true">
											<div class="modal-dialog">
												<div class="modal-content">
													<div class="modal-body">
														<button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
														<img src="images/blog/3.jpg" alt="" />
														<h2>Lorem ipsum dolor sit amet</h2>
														<p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.</p><p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.</p>
													</div> 
												</div>
											</div>
										</div>
									</div>
									<div class="col-sm-4">
										<div class="single-blog">
											<img src="images/blog/2.jpg" alt="" />
											<h2>Lorem ipsum dolor sit amet</h2>
											<ul class="post-meta">
												<li><i class="fa fa-pencil-square-o"></i><strong> Posted By:</strong> John</li>
												<li><i class="fa fa-clock-o"></i><strong> Posted On:</strong> Apr 15 2014</li>
											</ul>
											<div class="blog-content">
												<p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.</p>
											</div>
											<a href="" class="btn btn-primary" data-toggle="modal" data-target="#blog-six">Read More</a>
										</div>
										<div class="modal fade" id="blog-six" tabindex="-1" role="dialog" aria-hidden="true">
											<div class="modal-dialog">
												<div class="modal-content">
													<div class="modal-body">
														<button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
														<img src="images/blog/2.jpg" alt="" />
														<h2>Lorem ipsum dolor sit amet</h2>
														<p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.</p><p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.</p>
													</div> 
												</div>
											</div>
										</div>
									</div>

									<div class="col-sm-4">
										<div class="single-blog">
											<img src="images/blog/1.jpg" alt="" />
											<h2>Lorem ipsum dolor sit amet</h2>
											<ul class="post-meta">
												<li><i class="fa fa-pencil-square-o"></i><strong> Posted By:</strong> John</li>
												<li><i class="fa fa-clock-o"></i><strong> Posted On:</strong> Apr 15 2014</li>
											</ul>
											<div class="blog-content">
												<p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.</p>
											</div>
											<a href="" class="btn btn-primary" data-toggle="modal" data-target="#blog-seven">Read More</a>
										</div>
										<div class="modal fade" id="blog-seven" tabindex="-1" role="dialog" aria-hidden="true">
											<div class="modal-dialog">
												<div class="modal-content">
													<div class="modal-body">
														<button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
														<img src="images/blog/1.jpg" alt="" />
														<h2>Lorem ipsum dolor sit amet</h2>
														<p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.</p><p>Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.</p>
													</div> 
												</div>
											</div>
										</div>
									</div> 
								</div> 
							</div> 
						</section> <!--/#blog-->

						<section id="contact">
							<div class="container">
								<div class="row text-center clearfix">
									<div class="col-sm-8 col-sm-offset-2">
										<div class="contact-heading">
											<h2 class="title-one">Contact With Us</h2>
											<p></p>
										</div>
									</div>
								</div>
							</div>
							<div class="container">
								<div class="contact-details">
									<div class="pattern"></div>
									<div class="row text-center clearfix">
										<div class="col-sm-6">
											<div class="contact-address">
											<address><p><span>Departemen </span>Sistem Informasi</p><strong>
											Sekretariat <br>
											Lt. 2 Gd. Lama FTIf ITS <br>
											Institut Teknologi Sepuluh Nopember, Sukolilo <br>
											Surabaya, 60111 <br>
											Indonesia <br>
											Phone: +62 31 5999944 <br> </strong>
											</address>
											</div>
										</div>
										<div class="col-sm-6"> 
											<div id="contact-details">
												<div class="status alert alert-success" style="display: none"></div>
												<div class="contact-address"><address><p>Sisfor <span>KP</span></p><strong>Jam Buka : 08.00-16.00<br> +62 31 5999944</strong><br></address>
												<div class="social-icons">
													<a href="https://ms-my.facebook.com/pages/Jurusan-Sistem-Informasi-ITS/149235835122966" class="facebook external" data-animate-hover="shake"><i class="fa fa-facebook"></i></a>
									                <a href="https://www.instagram.com/hmsi_its/" class="instagram external" data-animate-hover="shake"><i class="fa fa-instagram"></i></a>
									                <a href="https://mail.google.com/mail/?view=cm&fs=1&tf=1&to=jurusan@is.its.ac.id&su=Hello&shva=1" class="email external" data-animate-hover="shake"><i class="fa fa-envelope"></i></a>
												</div>
												</div>
											</div>
										</div>
									</div>
								</div>
							</div> 
	</section> <!--/#contact--> 

	<footer id="footer"> 
		<div class="container"> 
			<div class="text-center"> 
				<p>Copyright &copy; 2017 - <a href="http://is.its.ac.id/">Information System Department</a> | ITS </p> 
			</div> 
		</div> 
	</footer> <!--/#footer--> 

	<script type="text/javascript" src="<?php echo base_url().'/asset_umum/js/jquery.js';?>"></script> 
	<script type="text/javascript" src="<?php echo base_url().'/asset_umum/js/bootstrap.min.js';?>"></script>
	<script type="text/javascript" src="<?php echo base_url().'/asset_umum/js/smoothscroll.js';?>"></script> 
	<script type="text/javascript" src="<?php echo base_url().'/asset_umum/js/jquery.isotope.min.js';?>"></script>
	<script type="text/javascript" src="<?php echo base_url().'/asset_umum/js/jquery.prettyPhoto.js';?>"></script> 
	<script type="text/javascript" src="<?php echo base_url().'/asset_umum/js/jquery.parallax.js';?>"></script> 
	<script type="text/javascript" src="<?php echo base_url().'/asset_umum/js/main.js';?>"></script> 
</body>
</html>

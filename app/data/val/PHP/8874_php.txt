
<?php
include 'connect.php';

session_start();

if(!$_SESSION['id'])
{
header("Location:index.php");
}

?>

<!DOCTYPE html>
<html lang="en">
  <head>
  
	<meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta name="description" content="">
    <meta name="author" content="Dashboard">
    <meta name="keyword" content="Dashboard, Bootstrap, Admin, Template, Theme, Responsive, Fluid, Retina">
	
    <title>Little Soldiers School</title>
  	<link href="assets/css/bootstrap.css" rel="stylesheet">
	 <link href="http://www.jqueryscript.net/css/jquerysctipttop.css" rel="stylesheet" type="text/css">
  
    <link type="text/css" rel="stylesheet" media="all" href="sampslider.css" />
    <style type="text/css">
    #slida-container{
      margin:30px auto;
			max-width:700px;
			max-height:576px;
		}
    </style>
	
    <!-- Bootstrap core CSS -->
    
    <!--external css-->
	 <link href="assets/custom.css" rel="stylesheet" />
    <link href="assets/font-awesome/css/font-awesome.css" rel="stylesheet" />
	<link href="assets/custom.css" rel="stylesheet" />
    <link rel="stylesheet" type="text/css" href="assets/css/zabuto_calendar.css">
    <link rel="stylesheet" type="text/css" href="assets/js/gritter/css/jquery.gritter.css" />
    <link rel="stylesheet" type="text/css" href="assets/lineicons/style.css">    
	 
    
    <!-- Custom styles for this template -->
    <link href="assets/css/style.css" rel="stylesheet">
    <link href="assets/css/style-responsive.css" rel="stylesheet">

    <script src="assets/js/chart-master/Chart.js"></script>
    
    <!-- HTML5 shim and Respond.js IE8 support of HTML5 elements and media queries -->
    <!--[if lt IE 9]>
      <script src="https://oss.maxcdn.com/libs/html5shiv/3.7.0/html5shiv.js"></script>
      <script src="https://oss.maxcdn.com/libs/respond.js/1.4.2/respond.min.js"></script>
    <![endif]-->
  </head>

  <body>

  <section id="container" >
      <!-- **********************************************************************************************************************************************************
      TOP BAR CONTENT & NOTIFICATIONS
      *********************************************************************************************************************************************************** -->
      <!--header start-->
     
	  <header class="row">
           
			
		<nav class ="navbar navbar-inverse navbar-fixed-top role = "navigation">
			 <div class="sidebar-toggle-box">
                  <div class="fa fa-bars tooltips" data-placement="right" data-original-title="Toggle Navigation"></div>
              </div>
            <div>
	
			<!--logo start-->
            <a  class="logo"><b> Little Soldiers School - Parent Alert Syestem</div></b></a>

            <!--logo end-->
            
            <div class="top-menu">
            	<ul class="nav navbar-right top-nav">
				  
              <button class="btn btn-warning" type="button">
                          <a href="logout.php"><span style="color:white"> Logout <i class="glyphicon glyphicon-off"> </i></span></a>
                            </button>          	</ul>

                                <ul class="nav navbar-right top-nav">

            </div>
			</div>
			</nav>

        </header>
				
      <!--header end-->
      
      <!-- **********************************************************************************************************************************************************
      MAIN SIDEBAR MENU
      *********************************************************************************************************************************************************** -->
      <!--sidebar start-->
     
	  <aside>
          <div id="sidebar"  class="nav-collapse ">
            
              <ul class="sidebar-menu" id="nav-accordion">
              
 
              	  
				  
	
              	  	
                  <li class="sub-menu">
                      <a class="active"  href="userhome.php">
                          <i class="li_shop"></i>
                         <span class ="active">Home</span>
                      </a>
                  </li> 

                  <li class="sub-menu">
                      <a  href="javascript:;" >
                          <i class="li_mail"></i>
                          <span>Compose</span>
                      </a>
                      <ul class="sub">
                          <li ><a  href="mycompose2.php">New Event Message</a></li>
						     <li ><a  href="usersimple.php">Simple Message</a></li>
                       
                          
                      </ul>
                  </li> 
				  
				  
				  

              </ul>
           
          </div>
      </aside>
	 
      <!--sidebar end-->
      
      <!-- **********************************************************************************************************************************************************
      MAIN CONTENT
      *********************************************************************************************************************************************************** -->
      <!--main content start-->
        <section id="main-content">
          <section class="wrapper">
      
  
  <div class="row">


  <article class="col-md-7 col-md-offset-1">

  <div id="slida-container" style="margin-top:10px;margin-left:-52px;">  
    <div id="slida-1" class="samp-slider samp-slider-mask">    
      <ul class="samp-container-horizontal">
      <li class="samp-container"><img src="imgs/im1.jpg"></li>
        <li class="samp-container"><img src="imgs/im2.jpg"></li>
        <li class="samp-container"><img src="imgs/im3.jpg"></li>
        <li class="samp-container"><img src="imgs/im4.jpg"></li>
        
        <li class="samp-container"><img src="imgs/im5.jpg"></li>
    <li class="samp-container"><img src="imgs/im6.jpg"></li>
    <li class="samp-container"><img src="imgs/im7.jpg"></li>
 
      </ul>
    </div>
  </article>

  <div class="col-md-offset-2">
  <div class="col-md-4">
   <div class="thumbnail" style="color:black;background-color:rgb(210,210,210)">
   <div class="caption">
   <p class="centered" style="font-family:Magneto;font-size:20px;" ><i class="glyphicon glyphicon-flag"> </i> About Us </p>
   <p style="font-family:Monotype Corsiva;font-size:18px;">We the LIITTLE SOLDIERS is an organization working for the development of a child at his initial years.A child with just some months old arrived on this planet must be handle with care and love but also in a discilpilned manner.Estabilshed on 1992 by   Mrs. Prabha Rajput  she saw a dream to make little and brave soldiers from this institute who can  
   fight all against the odds in the world when they grow up and do respect of all. </p>
  </div>
  </div>

  </div>
  </div>

</div>

 <div class="col-md-offset-1">
  <div class="col-md-4">
   <div class="thumbnail" style="color:black;background-color:rgb(210,210,210)">
   <div class="caption" style="color:black">
 
   <p style="font-family:Monotype Corsiva;font-size:18px;"><i class="glyphicon glyphicon-pushpin"> </i> Discilpline is something that will make you a well bahaved and successfull person loved and honoured by all.</p>
    <p class="text-right" style="font-family:Magneto;font-size:18px;" ><i class="li li_pen"> </i>Prabha Rajput </p>
     <p class="text-right" style="font-family:Magneto;font-size:14px;" >Founder</p>

  </div>
  </div>

  </div>
  </div>

  <div class="col-md-offset-1">
  <div class="col-md-4">
   <div class="thumbnail" style="color:black;background-color:rgb(210,210,210)">
   <div class="caption">

   <p class="centered" style="font-family:Magneto;font-size:18px;" ><i class="glyphicon glyphicon-send"> </i> Aim </p>
    
 
   <p style="font-family:Monotype Corsiva;font-size:18px;">We have a clear objective in mind to make masters from our school that will perform best out of themselves in any situation and achieve their goals with passion.  </p>
    

  </div>
  </div>

  </div>

  <div class="col-md-offset-1">
  <div class="col-md-4">
   <div class="thumbnail" style="background-color:rgb(210,210,210)">
   <div class="caption">
 
   <p style="font-family:Monotype Corsiva;font-size:18px;"> <i class="glyphicon glyphicon-pushpin"> </i>A person's nature defines how well he is going to be recieved, aceepted  and respected by this world.</p>
    <p class="text-right" style="font-family:Magneto;font-size:18px;" ><i class="li li_pen"> </i>Vina Sharma</p>
     <p class="text-right" style="font-family:Magneto;font-size:14px;" >Head Faculty</p>

  </div>
  </div>
  </div>

  </div>
  </div>

  </div>


  



  </div>



  <?php

  $p = 0;

$q = "SELECT *  FROM `msg_count` WHERE `id` = '1'";

$k1;
$k2;
$k3;$k4;$k5;$k6;$k7;$k8;$k9;$k10;$k11;$k12;

$run = mysql_query($q);

for($i = 1; $i <= 12; $i ++)

{

if($i == 1)
{
 $k1 = mysql_result($run, $p, 'msg_jan');


$_SESSION['jan'] = $k1;


}

else if($i == 2)
{
  $k2 = mysql_result($run, $p, 'msg_feb');

  
   $_SESSION['feb'] = $k2;

}



else if($i == 3)
{
 $k3 = mysql_result($run, $p, 'msg_mar');


$_SESSION['mar'] = $k3;


}

else if($i == 4)
{
  $k4 = mysql_result($run, $p, 'msg_apr');

    $_SESSION['apr'] = $k4;

}


if($i == 5)
{
 $k5 = mysql_result($run, $p, 'msg_may');


$_SESSION['may'] = $k5;


}

else if($i == 6)
{
  $k6 = mysql_result($run, $p, 'msg_jun');

    
    $_SESSION['jun'] = $k6;

}


if($i == 7)
{
 $k7 = mysql_result($run, $p, 'msg_jul');

$_SESSION['jul'] = $k7;


}

else if($i == 8)
{
  $k8 = mysql_result($run, $p, 'msg_aug');

    
    $_SESSION['aug'] = $k8;

}
else if($i == 9)
{
  $k9 = mysql_result($run, $p, 'msg_sep');

        $_SESSION['sep'] = $k9;

}
else if($i == 10)
{
  $k10 = mysql_result($run, $p, 'msg_oct');

      $_SESSION['oct'] = $k10;

}
else if($i == 11)
{
  $k11 = mysql_result($run, $p, 'msg_nov');

    
     $_SESSION['nov'] = $k11;

}
else if($i == 12)
{
  $k12 = mysql_result($run, $p, 'msg_dec');

     $_SESSION['dec'] = $k12;

}




}


 
$_SESSION['k_tot'] = $k1+$k2+$k3+$k4+$k5+$k6+$k7+$k8+$k9+$k10+$k11+$k12;  


  ?>

  
<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"></script>
  <script type="text/javascript" src="sampslider.js?v1"></script>
  <script type="text/javascript">
  
    var slida = jQuery('#slida-1').sampSlider({
      aspectratio:1.8,
      autosize: true,
      autoplay: 4000,
      slidespeed: 2500,
      dragonbar: true,
      startslide: 1
    });
  
  </script>
    <script type="text/javascript">

  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-36251023-1']);
  _gaq.push(['_setDomainName', 'jqueryscript.net']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();

</script>
      

  
   </article>
    </section>          <!--wrapper -->
      </section>   
            

      
      <!--footer end-->-2
  </section>

    <!-- js placed at the end of the document so the pages load faster -->
   
    <script src="assets/js/bootstrap.min.js"></script>
    <script class="include" type="text/javascript" src="assets/js/jquery.dcjqaccordion.2.7.js"></script>
    <script src="assets/js/jquery.scrollTo.min.js"></script>
    <script src="assets/js/jquery.nicescroll.js" type="text/javascript"></script>
    <script src="assets/js/jquery.sparkline.js"></script>
   

    <!--common script for all pages-->
    <script src="assets/js/common-scripts.js"></script>
    
    <script type="text/javascript" src="assets/js/gritter/js/jquery.gritter.js"></script>
    <script type="text/javascript" src="assets/js/gritter-conf.js"></script>
  <!-- <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js"></script> -->
   
  
  
  
  <script type="application/javascript">
        $(document).ready(function () {
            $("#date-popover").popover({html: true, trigger: "manual"});
            $("#date-popover").hide();
            $("#date-popover").click(function (e) {
                $(this).hide();
            });
        
            $("#my-calendar").zabuto_calendar({
                action: function () {
                    return myDateFunction(this.id, false);
                },
                action_nav: function () {
                    return myNavFunction(this.id);
                },
                ajax: {
                    url: "show_data.php?action=1",
                    modal: true
                },
                legend: [
                    {type: "text", label: "Special event", badge: "00"},
                    {type: "block", label: "Regular event", }
                ]
            });
        });
        
        
        function myNavFunction(id) {
            $("#date-popover").hide();
            var nav = $("#" + id).data("navigation");
            var to = $("#" + id).data("to");
            console.log('nav ' + nav + ' to: ' + to.month + '/' + to.year);
        }
    
    
    </script>
  

  </body>
</html>

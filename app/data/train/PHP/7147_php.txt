<?php defined('BASEPATH') or exit('No direct script access allowed');

/**
 * User: Nitesh
 * Date: 20/10/15
 * Time: 11:05 PM
 */ 
//print_r($data);
?>


<?php if($this->session->flashdata('msg')){ ?>
  <?php  echo ' <div class="alert alert-success " role="alert">'.$this->session->flashdata('msg').'</div>'; }?>
<section id="featured" >
	 
  
	<!-- Slider -->
        <div id="main-slider" class="flexslider">
            <ul class="slides">
              <li>
                <img src="<?php echo base_url();?>assets\img\slides\1.jpg" alt="">
                <div class="flex-caption">
                    <h3> Quantitative and Qualitative services</h3> 
					<!-- <p>Doloribus omnis minus temporibus perferendis ipsa architecto non, magni quam</p>   -->
                </div>
              </li>
              <li>
                <img src="<?php echo base_url();?>assets\img\slides\2.jpg" alt="">
                <div class="flex-caption">
                    <h3>Strong network base</h3> 
					<!-- <p>Lorem ipsum dolor sit amet, consectetur adipisicing elitincidunt eius magni provident.</p>  -->
                </div>
              </li>
              <li>
                <img src="<?php echo base_url();?>assets\img\slides\3.jpg" alt="">
                <div class="flex-caption">
                    <h3> Full regional project management</h3> 
					<!-- <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit donec mer lacinia.</p>   -->
                </div>
              </li>
            </ul>
        </div>
	<!-- end slider -->
 
	</section>

	<!-- ..................About. us......................... -->

<section id="content">
	
	<div class="container">
	<div class="row">
								<div class="col-md-8" >
								 <h3> About Us</h3>
								<p style="font-size: 16px;word-spacing: 2.5px;text-align: justify;">
									Research Pacific India is part of The Research Pacific Group (TRPG) which is an independent, owner-managed marketing research and consultancy services organization headquartered at Singapore.  Founded in 1988, the Group at present owns companies in 6 countries viz. Singapore, Malaysia, Hong Kong, South Korea, Japan and India. It is represented in 6 other Asian countries through approved associates. Group has unparalleled Asia Pacific expertise with this well-established chain of partners, each having its specific understanding of their individual markets. All projects are directed centrally from Singapore HQ, enabling clients to have one- stop interaction. 

								</p>
								<br>
								<p style="font-size: 16px;word-spacing: 2.5px;text-align: justify;">
									Research Pacific India (RPI) commenced operations in 1998 and since then has been providing solutions to the challenges facing both local and international companies. RPI is based in New Delhi and also has an office in Mumbai. The Company has in-house research, field and analysis capabilities. RPI provides nation-wide field coverage through a network of approved associates based at strategically located cities from time-to-time. 

								</p>
									<?php  

									/*$about=substr($about[0]['rpi_about_1'],'0','1000');
                                     echo $about;*/

									 ?>
								</div>
								<div class="col-md-4" style="border-left: 1px solid #F7F3F4; ">
 				                      <h3> Contacts Us</h3>
									<div class="contact-form">
										<form action="<?php echo base_url().'users/contactUs'; ?>" method="post" id="contact-form" role="form" novalidate="novalidate">
											<div class="form-group has-feedback">
												<label for="name">Name*</label>
												<input type="text" class="form-control" id="name" name="name" placeholder="">
												<i class="fa fa-user form-control-feedback"></i>
											</div>
											<div class="form-group has-feedback">
												<label for="email">Email*</label>
												<input type="email" class="form-control" id="email" name="email" placeholder="">
												<i class="fa fa-envelope form-control-feedback"></i>
											</div>
											<div class="form-group has-feedback">
												<label for="mobile">Mobile No*</label>
												<input type="text" class="form-control" id="mobile" name="mobile" placeholder="">
												<i class="fa fa-navicon form-control-feedback"></i>
											</div>
											<div class="form-group has-feedback">
												<label for="message">Message*</label>
												<textarea class="form-control" rows="6" id="message" name="message" placeholder=""></textarea>
												<i class="fa fa-pencil form-control-feedback"></i>
											</div>
											<input type="submit" value="Submit" class="btn btn-default">
										</form>
									</div>
							 </div>
	 </div>
	</div>
 
	</section>
	<hr>


	<!-- ..................About. us..end....................... -->

<!-- ..................services....................... -->
<section id="content">
		<div class="container content">		
        <!-- Service Blcoks -->
        <div class="row service-v1 margin-bottom-40">
            <div class="col-md-3 md-margin-bottom-40">
               <img class="img-responsive" src="<?php echo base_url();?>assets\img\1.jpg" alt="">   
                <h3>Advertising Research & Testing</h3>
                <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit. Dolores quae porro consequatur aliquam, incidunt eius magni provident, doloribus omnis minus temporibus perferendis nesciunt quam repellendus  </p>        
            </div>
            <div class="col-md-3">
                <img class="img-responsive" src="<?php echo base_url();?>assets\img\2.jpg" alt="">            
                <h3>Internet / Web Research </h3>
                <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit. Dolores quae porro consequatur aliquam, incidunt eius magni provident, doloribus omnis minus temporibus perferendis nesciunt quam repellendus </p>        
            </div>
            <div class="col-md-3 md-margin-bottom-40">
              <img class="img-responsive" src="<?php echo base_url();?>assets\img\3.png" alt="">  
                <h3>Agri-related Research</h3>
                <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit. Dolores quae porro consequatur aliquam, incidunt eius magni provident, doloribus omnis minus temporibus perferendis nesciunt quam repellendus </p>        
            </div>
            <div class="col-md-3 md-margin-bottom-40">
              <img class="img-responsive" src="<?php echo base_url();?>assets\img\4.jpg" alt="">  
                <h3>Market Segmentation</h3>
                <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit. Dolores quae porro consequatur aliquam, incidunt eius magni provident, doloribus omnis minus temporibus perferendis nesciunt quam repellendus</p>        
            </div>
        </div>
     <div class="row service-v1 margin-bottom-40">
            <div class="col-md-3 md-margin-bottom-40">
               <img class="img-responsive" src="<?php echo base_url();?>assets\img\5.png" alt="">   
                <h3>Automotive Research </h3>
                <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit. Dolores quae porro consequatur aliquam, incidunt eius magni provident, doloribus omnis minus temporibus perferendis nesciunt quam repellendus </p>        
            </div>
            <div class="col-md-3">
                <img class="img-responsive" src="<?php echo base_url();?>assets\img\6.jpg" alt="">            
                <h3>Medical / Pharma Research </h3>
                <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit. Dolores quae porro consequatur aliquam, incidunt eius magni provident, doloribus omnis minus temporibus perferendis nesciunt quam repellendus </p>        
            </div>
            <div class="col-md-3 md-margin-bottom-40">
              <img class="img-responsive" src="<?php echo base_url();?>assets\img\7.jpg" alt="">  
                <h3>B2B and industrial Research  </h3>
                <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit. Dolores quae porro consequatur aliquam, incidunt eius magni provident, doloribus omnis minus temporibus perferendis nesciunt quam repellendus </p>        
            </div>
            <div class="col-md-3 md-margin-bottom-40">
              <img class="img-responsive" src="<?php echo base_url();?>assets\img\8.jpg" alt="">  
                <h3>Opinion Polls</h3>
                <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit. Dolores quae porro consequatur aliquam, incidunt eius magni provident, doloribus omnis minus temporibus perferendis nesciunt quam repellendus </p>        
            </div>
        </div>


      </div>
  </section>    
  <hr>  
<!-- .......................................company Address
 -->
 <section id="content">
	
	<div class="container">
	<div class="row">
								<div class="col-md-6">
									
                               <h3> Company Address</h3>
                                <p> 301, 3rd floor, 5 Pusa Road, New Delhi<br>
Contact Person: James Washington <br> 
Contact number: 9811926888 <br>
Contact Email Id: jamesrpi@respac.com
</p>

								</div>
								<div class="col-md-6">
 <script type="text/javascript" src="http://maps.google.com/maps/api/js?sensor=false"></script>
<div style="overflow:hidden;height:300px;width:600px;"><div id="gmap_canvas" style="height:300px;width:600px;">
	
</div><style>#gmap_canvas img{max-width:none!important;background:none!important}</style>
<a class="google-map-code" href="http://www.trivoo.net" id="get-map-data">trivoo</a></div>
<script type="text/javascript"> function init_map(){var myOptions = {zoom:14,center:new google.maps.LatLng(28.549291,77.2678137),mapTypeId: google.maps.MapTypeId.ROADMAP};map = new google.maps.Map(document.getElementById("gmap_canvas"), myOptions);marker = new google.maps.Marker({map: map,position: new google.maps.LatLng(28.549291,77.2678137)});infowindow = new google.maps.InfoWindow({content:"<b>Okhla</b><br/>Phase-II<br/> New Delhi" });google.maps.event.addListener(marker, "click", function(){infowindow.open(map,marker);});infowindow.open(map,marker);}google.maps.event.addDomListener(window, 'load', init_map);</script>
								
							 </div>
							</div>
	</div>
 
	</section>
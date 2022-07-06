</div><!-- close .main-content -->

<footer id="colophon" class="site-footer" role="contentinfo" style="padding-top: 20pt; padding-bottom: 40pt;">
	<div class="container">
		<div class="row">
			<div class="col-xs-0 col-sm-3">
			</div>
			<div class="col-xs-6 col-sm-3">
				<h5><a href="https://www.facebook.com/thyspaoxford/">Facebook</a></h5>
				<h5><a href="https://www.instagram.com/thy.spa/">Instagram</a></h5>
				<h5><a href="https://twitter.com/thyspaoxford">Twitter</a></h5>
				<h5>To receive latest news and offers, please <a href="http://eepurl.com/bS2S8j">subscribe</a> to our mailing list.</h5>
			</div>
			<div class="col-xs-6 col-sm-3">
				<h4 class="ts-logo-font ts-capitalize">
					<span>Thy</span>
					<span>Spa</span>
				</h4>
				<h5>Original Thai Massage</h5>
				<h5><a href="https://www.google.co.uk/maps/place/Thy+Spa/@51.74923,-1.2445757,17z/data=!3m1!4b1!4m5!3m4!1s0x4876c14da21a6c03:0x67617b09ef70e45e!8m2!3d51.74923!4d-1.242387?hl=en" target="_blank">34 Cowley Road, Oxford, OX4 1HZ</a></h5>
				<h5>E-mail: <a href="mailto:oxford@thyspa.com">oxford@thyspa.com</a></h5>
				<h5>Phone: +44 1865 497999</h5>
				<h5>Opening Hours</h5>
				<h5>Monday - Saturday | 9:00am - 9:00pm</h5>
				<h5>Sunday | 9:00am - 7:00pm</h5>
			</div>
			<div class="col-xs-0 col-sm-3">
			</div>
		</div>
	</div>
</footer>

<script>
function resizeShedulWidget() {
  var height = window.innerHeight - 90;
  if (height < 200) height = 200;
  d3.select("#shedulWidget").style("height", height + "px");
}

resizeShedulWidget();

window.addEventListener('resize', function(event){
  resizeShedulWidget();
});
</script>

<?php wp_footer(); ?>

</body>
</html>

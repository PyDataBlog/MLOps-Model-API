jQuery(document).ready(function(){
	jQuery('.carousel').carousel()

    var FPS = 30;

	var player = $('#player')

	var pWidth = player.width();

	$window = $(window)
	var wWidth  = $window.width();

	setInterval(function() {
		update();
	}, 1000/FPS);


	function update() {
		if(keydown.space) {
			player.shoot();
		}

		if(keydown.left) {
			console.log('go left')

			player.css('left', '-=10');

		}

		if(keydown.right) {
			console.log('go right')
		
			var x = player.position().left;

			if(x + pWidth > wWidth)
			{
				player.css('left', '0')
			}
			else if(x < 0 )
			{
				var p = wWidth + x - pWidth;
				var t = p + 'px'
				player.css('left', t)
			}
			else {
				player.css('left', '+=10');
			}



		}

	}

	$('')

})

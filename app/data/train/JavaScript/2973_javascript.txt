$(document).ready(function(){
  'use strict';
	

	//Turn off and on the music
	$("#sound-control").click(function() {
		var toggle = document.getElementById("sound-control");
		var music = document.getElementById("music");
		
		if(music.paused){
			music.play();
			$("#sound-control").attr('src', 'img/ljud_pa.png');
		} else {
			music.pause();
			$("#sound-control").attr('src', 'img/ljud_av.png');
		}
	});
	
	
	//The slideshow 
	var started = false;
	
	//Backwards navigation
	$("#back").click(function() {
		stopSlideshow();
		navigate("back");
	});

	//Forward navigation
	$("#next").click(function() {
		stopSlideshow();
		navigate("next");
	});
	
	
	var interval;
	
	$("#control").click(function(){ 
        if(started)
        {
          stopSlideshow();
        }
        else
        {
          startSlideshow();
        }
    });
	
	
	var activeContainer = 1;
	var currentImg = 0;
	var animating = false;
	var navigate = function(direction) {
		
		//Check if no animation is running
		if(animating) {
			return;
		}
		
		//Check wich current image we need to show
		if(direction == "next") {
			currentImg++;
			if(currentImg == photos.length + 1) {
				currentImg = 1;
			}
		} else {
			currentImg--;
			if(currentImg == 0) {
				currentImg = photos.length;
			}
		}
		
		//Check wich container we need to use
		var currentContainer = activeContainer;
		if(activeContainer == 1) {
			activeContainer = 2;
		} else {
			activeContainer = 1;
		}
		
		showImage(photos[currentImg - 1], currentContainer, activeContainer);
	};
	
	
	var currentZindex = -1;
	var showImage = function(photoObject, currentContainer, activeContainer) {
		animating = true;
		
		//Make sure the new container is always on the background
		currentZindex--;
		
		//Set the background image of the new active container
		$("#slideimg" + activeContainer).css({
			"background-image" : "url(" + photoObject.image + ")",
			"display" : "block",
			"z-index" : currentZindex
		});
		
		//Fade out and hide the slide-text when the new image is loading
		$("#slide-text").fadeOut();
		$("#slide-text").css({"display" : "none"});
		
		//Set the new header text
		$("#firstline").html(photoObject.firstline);
		$("#secondline")
			.attr("href", photoObject.url)
			.html(photoObject.secondline);
		
		//Fade out the current container
		//and display the slider-text when animation is complete
		$("#slideimg" + currentContainer).fadeOut(function() {
			setTimeout(function() {
					$("#slide-text").fadeIn();
					animating = false;
			}, 500);
		});
	};
	
	
	var stopSlideshow = function() {
		
		//Change the background image to "play"
		$("#control").css({"background-image" : "url(img/play.png)" });
		
		//Clear the interval
		clearInterval(interval);
		
		started = false;
	};
	
	
	var startSlideshow = function() {
		$("#control").css({ "background-image" : "url(img/pause.png)" });
		navigate("next");
		interval = setInterval(function() { navigate("next"); }, slideshowSpeed);
		started = true;
    };   
	
	
	$.preloadImages = function() {
		$(photos).each(function() {
			$('<img>')[0].src = this.image;
		});
		startSlideshow();
	}

	$.preloadImages();
});
//Settings
actype = ['image/png','image/jpeg','image/jpg','image/gif']; /* Accepted mime type */
maxweight = 819200; /* Max file size in octets */
maxwidth = 150; /* Max width of the image */
maxheight = 150; /* Max height*/

//Caching variable selector
ish = $('.ish'); /* On attach element hide or show */
msgUp = $('.msgUp'); /* container message, infos, error... */
filezone = $('.filezone'); /* Selector filezone label */
fileprev = $('.filezone').children('img'); /* Selector img element */
filesend = $('#filesend'); /* Selector input file (children label) */
fileup = $('#fileup'); /* Selector button submit */
reset = $('#reset'); /* Selector button reset */

ish.hide(); /* Initial hide */


$(':file').change(function(e) {

	//Cancel the default execution
	e.preventDefault();

	//Full file
	file = this.files[0];

	var filer = new FileReader;

	filer.onload = function() {

    	//Get size and type
    	var aType = file.type;
        var aSize = file.size;

        //Check the file size
        if(aSize > maxweight) {
        	msgUp.text('To large, maximum'+ maxweight +' bytes');
        	return;
        }

        //Check the file type
        if($.inArray(aType, actype) === -1) {
        	msgUp.text('File type not allowed');
        	return;
        }

        //Set src / preview
    	fileprev.attr('src', filer.result);

        //Make new Image for get the width / height
    	var image = new Image();
      	image.src = filer.result;

		image.onload = function() {

		//Set width / height
		aWidth  = image.width;
		aHeight = image.height;
		
		//Check width
		if(aWidth > maxwidth) {
			msgUp.text('Maximum' + maxwidth +' width allowed');
	        	return;
		}

		//Check height
		if(aHeight > maxheight) {
			msgUp.text('Maximum' + maxheight +' height allowed');
	        	return;
			
		}

		//Success of every check, display infos about the image and show up the <img> tag
		msgUp.html('Size :'+ aSize +' bytes<br>Filetype : '+ aType +'<br>Width :'+ aWidth +' px<br>Height: '+ aHeight +' px');
			ish.show();
			filesend.addClass('lock').css('height','0%');

		//End image
		};

	//End filer
    	};

    //File is up
    filer.readAsDataURL(file);
});

//input file prevent on lock
$(document).off('click', '#filesend');
$(document).on('click', '#filesend',  function(e) {

	//Cancel the default execution if img ready to be send to php
	if($(this).hasClass('lock'))
		e.preventDefault();

});

//On reset
$(document).off('click', '#reset');
$(document).on('click', '#reset',  function(e) {

	//Cancel the default execution
	e.preventDefault();

	//Remove the href link
	fileprev.attr('src', '');

	//Set default message
	msgUp.text('Drop your avatar !');

	//Remove the lock of the input file
	if(filesend.hasClass('lock'))
		filesend.css('height','100%').removeClass();

	//Set default  reset value
	$(this).val('Clear');

	//Back to hide
	ish.hide();
});

//On fileup
$(document).off('click', '#fileup');
$(document).on('click', '#fileup', function(e) {

	//Cancel the default execution
	e.preventDefault();

	//Set variable which contain the entiere form / field
	var filesfm = new FormData(document.querySelector("form"));

	$.ajax({
        url: 'upload.php',  //Server side script (php...)
        type: 'POST',
        data:filesfm,
        processData: false,  //Avoid jquery process
        contentType: false   //Avoid set content type (done by var filesfm)
    	}).done(function(msg) {

    		//Hide the button upload
    		fileup.hide();

    		//Change the text reset button  (make as reinitialize the form)
    		reset.val('Upload again !');

    		//On success upload
    		if(msg === 'err')
    			msgUp.text('Something went wrong, try again.'); //That should not happen !
    		else
    			msgUp.text('Success, your file is available '+ msg);  //Add the url of your file except the filename generated

	}); 

});

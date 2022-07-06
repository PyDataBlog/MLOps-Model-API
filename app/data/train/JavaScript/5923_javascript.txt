'use strict';

// Call this function when the page loads (the "ready" event)
$(document).ready(function() {
	initializePage();
})

/*
 * Function that is called when the document is ready.
 */
function initializePage() {
	$('.project a').click(addProjectDetails);

	$('#colorBtn').click(randomizeColors);

	$('#apibutton').click(getExternalApi);
}

/*
 * Make an AJAX call to retrieve project details and add it in
 */
function addProjectDetails(e) {
	// Prevent following the link
	e.preventDefault();

	// Get the div ID, e.g., "project3"
	var projectID = $(this).closest('.project').attr('id');
	// get rid of 'project' from the front of the id 'project3'
	var idNumber = projectID.substr('project'.length);

	console.log("User clicked on project " + idNumber);

	$.get('/project/' + idNumber, addProject);
	console.log('/project/' + idNumber);
}

function addProject(result) {
  console.log(result);
  var projectHTML = '<a href="#" class="thumbnail">' +
    '<img src="' + result['image'] + '" class="detalsImage">' +
    '<p>' + result['title'] + '</p>' +
    '<p> <small>' + result['date'] + '</small></p>' +
    '<p>' + result['summary'] + '</p> </a>';

    $("#project" + result['id'] + " .details").html(projectHTML); 
}

/*
 * Make an AJAX call to retrieve a color palette for the site
 * and apply it
 */
function randomizeColors(e) {
	e.preventDefault();

	$.get('/palette', addColor);
}

function addColor(result) {
	console.log(result);

	var colors = result['colors']['hex'];

	$('body').css('background-color', colors[0]);
$('.thumbnail').css('background-color', colors[1]);
$('h1, h2, h3, h4, h5, h5').css('color', colors[2]);
$('p').css('color', colors[3]);
$('.project img').css('opacity', .75);
}

function getExternalApi(e) {
	e.preventDefault();

	$.get('https://api.spotify.com/v1/artists/4dpARuHxo51G3z768sgnrY', getAPI);
}

function getAPI(result) {
	console.log(result);

	var images = result['images'];
	console.log(images);

	var adeleHTML = '<h1>' + result['name'] + '</h1>' +
	'<p> popularity: ' + result['popularity'] + '</p>' +
	'<p> genre: ' + result['genres'][0] + '</p>' + 
	'<img src="' + images[2]['url'] + '"> <br />' ;

	$("#output").html(adeleHTML); 
}
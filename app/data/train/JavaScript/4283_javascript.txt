'use strict';
/* main App */
var app = angular.module('submitConformationcontroller', []);
app.controller('confirmationCtrl', ['$scope', function($scope){
	
	$scope.volunteerList = ["Joop Bakker", "Dirk Dijkstra", "Sterre Hendriks",
	 "Hendrik Jacobs", "Hans Heuvel", "Jaap Beek", "Jan-Jaap Dijk", 
	 "Marleen Jansen", "Geert Hoek", "Beer Heuvel"];


	$scope.jobTitle = '';
	$scope.jobType = '';
	$scope.describeWork = '';
	function getParameterByName(name) {
	    name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
	    var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
	        results = regex.exec(location.search);
	    return results == null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
	}
	$scope.jobTitle = getParameterByName('jobTitle');
	$scope.jobType = getParameterByName('jobType');
	$scope.describeWork = getParameterByName('describeWork');

}]);
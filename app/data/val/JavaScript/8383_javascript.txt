'use strict';

angular.module('wsapp')
    .controller('KnowledgeAddController', function ($scope, knowledgeService, $location, $state) {
        $scope.data={};
        $scope.program = function(){
        				   	alertify.success("Test");
        }
        $scope.know = function(){
        	console.log($scope.data);
        	knowledgeService.insertKnowledge($scope.data,
       			   function(response){

        				   console.log(response.data);
        				   //if(response.data.success==true){
        				   	alertify.success("SUCCESS");
        				   	$state.go('home');
        				   //}else{
        				   	//alertify.error("ERROR");
        				   //}

        			   }
        			   ,function(response){

        					console.log(response);
        					alertify.error("ERROR");

        			   });
        }
});
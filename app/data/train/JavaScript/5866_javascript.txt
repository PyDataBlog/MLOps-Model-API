'use strict';

angular.module('myApp.gear', ['ngRoute'])

.config(['$routeProvider', function($routeProvider) {
  $routeProvider.when('/gear', {
    templateUrl: 'gear/gear.html',
    controller: 'GearCtrl'
  });
}])

.controller('GearCtrl', ['$scope','$http', function($scope, $http) {
	$http.get('resources/data/gear.json').
    success(function(data, status, headers, config) {
      $scope.content = data;
    }).
    error(function(data, status, headers, config) {
      // log error
    });
	
}]);
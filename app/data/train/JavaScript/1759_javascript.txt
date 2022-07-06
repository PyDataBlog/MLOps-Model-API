angular.module('starter.controllers', [])


// A simple controller that fetches a list of data from a service
.controller('PetIndexCtrl', function($scope, PetService) {
  // "Pets" is a service returning mock data (services.js)
  $scope.pets = PetService.all();
})


// A simple controller that shows a tapped item's data
.controller('PetDetailCtrl', function($scope, $stateParams, PetService) {
  // "Pets" is a service returning mock data (services.js)
  $scope.pet = PetService.get($stateParams.petId);
})


// getting fake favor data 

.controller('FavorIndexCtrl', function($scope, FavorService) {
  $scope.favors = FavorService.all();
})


// A simple controller that shows a tapped item's data
.controller('FavorDetailCtrl', function($scope, $stateParams, FavorService) {
  // "Pets" is a service returning mock data (services.js)
  $scope.favor = FavorService.get($stateParams.favorId);
});

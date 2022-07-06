angular.module('starter.controllers', [])

// A simple controller that fetches a list of data from a service
.controller('JobIndexCtrl', function($rootScope, PetService) {
  // "Pets" is a service returning mock data (services.js)
  $rootScope.pets = PetService.all();
})

  // A simple controller that shows a tapped item's data
.controller('PetDetailCtrl', function($scope, $rootScope, $state, $stateParams, PetService) {
  $rootScope.pet = PetService.get($stateParams.jobId);
  $scope.goBack = $state.go('job.pet-index');
})

.controller('NewJobDetailCtrl', function($scope, $rootScope, $state, $stateParams, PetService) {
  $rootScope.pet = PetService.get($stateParams.jobId);
  $scope.goBack = $state.go('new-job');

})

.controller('LoginCtrl', function($scope, $state) {
  $scope.login = function () {
    $state.go("tab.adopt");
  };
})

.controller('JobCreationCtrl', function($scope, $rootScope, $state, $stateParams, $localstorage, PetService) {
  $scope.createJob = function () {
    var title = document.getElementById("title");
    var desc = document.getElementById("desc");
    var location = document.getElementById("location");

    if (title.value.trim() !== "" && desc.value.trim() !== "" && location.value.trim() !== "") {
      var newJobId = $localstorage.length() - 1;
      var newJob = {
        id: String(newJobId),
        title: String(title.value.trim()), 
        description: String(desc.value.trim()),
        location: String(location.value.trim()),

        quote: {
          damageDesc: "",
          estimatedCost: "", 
          estimatedTime: "",
        },

        report: {
          fixDesc: "",
          actualCost: "",
          startTime: "",
          finishTime: ""
        } 
      };

      $rootScope.pet = PetService.get(newJobId);
      $rootScope.pets = PetService.all(); 
      $localstorage.setObject(newJobId, newJob);
      $state.go('new-quote', {'jobId' : newJobId});
    }
  }
});



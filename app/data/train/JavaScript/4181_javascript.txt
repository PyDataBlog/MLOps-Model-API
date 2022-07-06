'use strict';

angular.module('user-filters')
    .directive('zoUserFilters', function(){
    return {
        restrict: 'E',
        replace: true,
        templateUrl: 'app/user-filters/list-directive.html',
        controller: 'UserFiltersDirectiveCtrl'
    }
})
    .controller('UserFiltersDirectiveCtrl', ['$scope', function($scope){

}]);



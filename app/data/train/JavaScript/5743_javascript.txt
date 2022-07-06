(function() {
  'use strict';

  function movieDetail(movieDetailService) {
    return {
      restrict: 'EA',
      replace: true,
      templateUrl: './src/app/movieDetail/template.html',
      scope: {},
      controllerAs: 'vm',
      bindToController: true,
      /*jshint unused:false*/
      controller: function($log, $stateParams) {
        var vm = this;

        movieDetailService.getMovie().then(function(response){
          vm.movie = response.data;
          vm.movie.vote = (vm.movie.vote_average*10);
          vm.movie.genres_name = [];
          vm.movie.production_companies_name = [];
          vm.movie.production_countries_name = [];
          for (var i = 0; i <= vm.movie.genres.length-1; i++) {
            vm.movie.genres_name.push(vm.movie.genres[i].name);
            vm.movie.genres_name.sort();
          }
          for (var i = 0; i <= vm.movie.production_companies.length-1; i++) {
            vm.movie.production_companies_name.push(vm.movie.production_companies[i].name);
            vm.movie.production_companies_name.sort();
          }
          for (var i = 0; i <= vm.movie.production_countries.length-1; i++) {
            vm.movie.production_countries_name.push(vm.movie.production_countries[i].name);
            vm.movie.production_countries_name.sort();
          }
        });
      },
      link: function(scope, elm, attrs) {
      }
    };
  }

  angular.module('movieDetailDirective', ['services.movieDetail'])
    .directive('movieDetail', movieDetail);
})();

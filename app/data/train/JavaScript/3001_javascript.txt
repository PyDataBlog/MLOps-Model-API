(function(){
  angular.module('list-products', [])
    .directive('productInfo', function() {
      return {
        restrict: 'E',
        templateUrl: 'partials/product-info.html'
      }
    })
    .directive('productForm', function() {
      return {
        restrict: 'E',
        templateUrl: 'partials/product-form.html',
        controller: function() {
          this.review = {};
          this.item = {mark:{}};
          this.addReview = function(item) {
            item.reviews.push(this.review);
            this.review = {};
          };
        },
        controllerAs: 'reviewCtrl',
        scope: {
          items: "=",
          marks: "="
        }
      };
    })
    .directive('productPanels', function() {
      return {
        restrict: 'E',
        templateUrl: 'partials/product-panels.html',
        controller: function(){
          this.tab = 1;
          this.selectTab = function(setTab) {
            this.tab = setTab;
          };
          this.isSelected = function(checkTab) {
            return checkTab === this.tab;
          };
        },
        controllerAs: 'panelCtrl',
        scope: {
          items: "=",
          marks: "="
        }
      };
    });
})();
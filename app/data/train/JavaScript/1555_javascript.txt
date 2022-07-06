function htmlEncode(value){
    return $('<div/>').text(value).html();
}
var app = angular.module("oasassets",[]).controller("snippetsController",function($scope){

    $scope.snippet = function(item){
        var elem = $("#"+item);
        var contents = elem.html().trim();
        elem.html(htmlEncode(contents));
        $('pre code').each(function(i, block) {
            hljs.highlightBlock(block);
        });

    };
    $scope.loadMenu = function(){
        $('#side-menu').metisMenu();
    };

});


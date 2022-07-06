(function () {
    "use strict";
    //angular.module('app', ['angularUtils.directives.dirPagination']);

    var env = {};

    if (window) {
        Object.assign(env, window.__env);
    }

    var app = angular.module("deviceManagement",
        ['angularUtils.directives.dirPagination',
            'common.services',
            'ui.router'//,
            //'deviceResourceRest'
            //'deviceResourceMock'

        ]);


    app.constant("rootUrl", "http://www.example.com");
    app.constant("$env", env);

    // configure state for device list    

    // add route state

    // reference the app variable 

    app.config(['$stateProvider',
        '$urlRouterProvider',
        function ($stateProvider, $urlRouterProvider) {   // pass in $stateProvider  -  and use array to be min safe
            $urlRouterProvider.otherwise("/");  //("/devices");  // default otherwise   if an activated state has no entry   

            $stateProvider
                .state("home", {
                    url: "/",
                    templateUrl: "app/welcomeView.html"
                })
                //manifest
                .state("manifestList", {
                    url: "/devices/manifest/:DeviceId",
                    templateUrl: "app/devices/manifestList.html",
                    controller: "ManifestCtrl as vm"
                })

                //devices
                .state("deviceList", {    // set url fragment 
                    url: "/devices", // http://localhost:8000/#/devices
                    templateUrl: "app/devices/deviceListView.html",  // url location   fetched template from web server
                    controller: "DeviceListCtrl as vm"   // associated controller is contructed
                })
                .state("deviceEdit", {
                    abstract: true,  // abstract set so that it cannot be explicitly activated , it must have child state 
                    url: "/devices/edit/:DeviceId",  // param is required which specific device id
                    templateUrl: "app/devices/deviceEditView.html",  // ui elements 
                    controller: "DeviceEditCtrl as vm",   // as with alias of vm
                    // resolve: {
                    //     deviceResource: "deviceResource",

                    //     device: function (deviceResource, $stateParams) {
                    //         var DeviceId = $stateParams.DeviceId;
                    //         return deviceResource.get({ DeviceId: DeviceId }).$promise;
                    //     }
                    // }
                })
                .state("deviceEdit.info", {
                    url: "/info",
                    templateUrl: "app/devices/deviceEditInfoView.html"
                })
                .state("deviceEdit.price", {
                    url: "/price",
                    templateUrl: "app/devices/deviceEditPriceView.html"
                })
                .state("deviceEdit.tags", {
                    url: "/tags",
                    templateUrl: "app/devices/deviceEditTagsView.html"
                })
                .state("deviceDetail", {
                    url: "/devices/:DeviceId",  // param is required which specific device id
                    templateUrl: "app/devices/deviceDetailView.html",  // ui elements 
                    controller: "DeviceDetailCtrl as vm"//{
                        //$scope.DeviceId = $stateParams.DeviceId;
                        //var DeviceId = $stateParams.DeviceId;
                    //}
                    
                    
                    //controller: "DeviceDetailCtrl as vm" //,  // as with alias of vm
                    // resolve: {  // resolve is a property of the stateconfiguration object
                    //     deviceResource: "deviceResource",    // key value pair     Key is  deviceResource   value is string name  of "deviceResource"

                    //     device: function (deviceResource, $stateParams) {      // $stateParams service is needed  because url: has this :DeviceId 
                    //         var DeviceId = $stateParams.DeviceId;
                    //         return deviceResource.get({ DeviceId: DeviceId }).$promise;   // function returns the promise 
                    //     }
                    // }
                    
                })

                //console.log('end .state  ui router');

        }]

    );

} ());
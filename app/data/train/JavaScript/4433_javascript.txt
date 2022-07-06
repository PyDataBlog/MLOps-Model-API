(function() {
    function config($stateProvider, $locationProvider) {
      $locationProvider
            .html5Mode({
                  enabled: true,
                  requireBase: false
            });

        $stateProvider
          .state('landing', {
                  url: '/',
                  controller: 'LandingCtrl as landing',
                  templateUrl: '/templates/landing.html'
            })

          .state('newroom', {
                  url: '/',
                  controller: 'NewRoomCtrl as newroom',
                  templateUrl: '/templates/newroom.html'
            })

          .state('login', {
                  url: '/',
                  controller: 'LoginCtrl as login',
                  templateUrl: '/templates/login.html'
          });
    }

    function BlocChatCookies($cookies,$uibModal) {
      if (!$cookies.blocChatCurrentUser || $cookies.blocChatCurrentUser === '') {
        this.animationsEnabled = true;
        $uibModal.open({
                animation: this.animationsEnabled,
                backdrop: 'static',
                templateUrl: '/templates/login.html',
                size: "sm",
                controller: "LoginCtrl",
                controllerAs: "login",
                });
        }
    }

  angular
    .module('blocChat',['ui.router','ui.bootstrap','firebase','ngCookies'])
    .config(config)
    .run(['$cookies','$uibModal', BlocChatCookies]);
})();

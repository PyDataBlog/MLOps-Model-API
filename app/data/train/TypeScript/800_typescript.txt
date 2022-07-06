import {name as headerControlledName} from './controller/HeaderController';

function config($stateProvider, $urlRouterProvider) {
    $urlRouterProvider.otherwise('/');

    $stateProvider
        .state('root', {
        url: "/",
        views: {
            header: {
                templateUrl: 'view/header.html',
                controller: headerControlledName
            },
            items: {
                templateUrl: 'view/items.html',
                controller: require('./controller/ItemsController').name
            },
            footer: {
                templateUrl: 'view/footer.html',
                controller: require('./controller/FooterController').name
            }
        }
    });
}

export default config;

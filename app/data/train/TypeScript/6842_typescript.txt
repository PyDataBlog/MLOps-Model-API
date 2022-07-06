import {IUrlRouterProvider} from "angular-ui-router";
import {ILocationProvider} from "angular";

/*@ngInject*/
export default function routing($urlRouterProvider:IUrlRouterProvider, $locationProvider:ILocationProvider) {
    $locationProvider.html5Mode(true);
    $urlRouterProvider.otherwise('/');
}

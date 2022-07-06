import controller from './controller';
import template from './template.pug';

routes.$inject = ['$stateProvider', '$urlRouterProvider'];

export default function routes($stateProvider, $urlRouterProvider){


	$stateProvider.state('main.item', {
		url: '/:id/item',
		template: template,
		controllerAs: 'ctrl',
		controller: controller
	})


}


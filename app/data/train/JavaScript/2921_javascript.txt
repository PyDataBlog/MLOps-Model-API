'use strict';

describe('heroList', function(){

//Load module that contains the heroList component
	beforeEach(module('heroList'));
	
	describe('HeroListController', function(){
		it('should create a `heroes` model with 6 heroes', inject(function($componentController){
			var ctrl = $componentController('heroList');
			
			expect(ctrl.heroes.length).toBe(6);
		}));
	});
});
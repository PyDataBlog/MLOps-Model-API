//filters.js  eventsApp.filter('durations'
'use strict';

describe('durations', function(){

	beforeEach(module("eventsApp"));

it('should return "Half Hour" when given a 1', inject(function(durationFilter){
	expect(durationFilter(1)).toEqual('Half Hour');
}))
it('should return "1 Hour" when given a 2', inject(function(durationFilter){
	expect(durationFilter(2)).toEqual('1 Hour');
}))
it('should return "Half Day" when given a 3', inject(function(durationFilter){
	expect(durationFilter(3)).toEqual('Half Day');
}))
it('should return " Hour" when given a 4', inject(function(durationFilter){
	expect(durationFilter(4)).toEqual('Full Day');
}))	
})

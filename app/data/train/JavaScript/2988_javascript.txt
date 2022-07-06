import Ember from 'ember';
import layout from './template';

export default Ember.Component.extend({
	layout: layout,
	classNames: ['kit-canvas-scroller'],

	canvasStyle: Ember.computed('parentView.canvasStyle', function() {
		return this.get('parentView').get('canvasStyle');
	}),
	numberOfItems: Ember.computed('parentView', function() {
		return this.$('.kit-canvas-content').children().length;
	}),
	willInsertElement: Ember.on('willInsertElement', function() {
		this.get('parentView').set('scroller', this);
	}),
  });

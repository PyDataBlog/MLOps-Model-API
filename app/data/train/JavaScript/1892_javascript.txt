import Ember from 'ember';

export default Ember.Component.extend({
  classNames: ['button-content'],
  audio: Ember.inject.service(),
  soundName: null,
  rate: 1,

  preloadSounds: function() {
    this.get('audio');
  }.on('init'),

  actions: {
    play() {
      this.get('audio').play(this.get('soundName'), this.get('rate'));
    }
  }
});

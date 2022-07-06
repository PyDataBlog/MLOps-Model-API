/**
 * The main purpose of this in my mind is for navigation
 * this way when this route is entered either via direct url
 * or by a link-to etc you send a ping so that nav can be updated
 * in the hierarchy.
 * 
 * curious about feedback. I have used something similar in practice
 * but it's mainly for keeping the ui correct and things like that
 * without tightly coupling things together or at least that is my
 * hope.
 */
export default Ember.Route.extend({
    beforeModel: function(trans) {
        trans.send('ping', this.routeName);
    }
});

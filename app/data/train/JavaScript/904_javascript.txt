var fastn = require('fastn')({
    _generic: require('fastn/genericComponent'),
    list: require('fastn/listComponent'),
    templater: require('fastn/templaterComponent'),
    text: require('fastn/textComponent'),
    ratingControl: require('./ratingControlComponent')
});

module.exports = function(settings){
    return fastn('ratingControl', settings).attach().render();
};
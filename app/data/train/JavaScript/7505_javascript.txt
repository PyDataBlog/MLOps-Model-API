export default Ember.Handlebars.makeBoundHelper(function(price) {
   var number = parseFloat(price/100).toFixed(2),
       dollar = number.split('.')[0],
       cents = number.split('.')[1],
       dollars = dollar.split('').reverse().join('')
        .replace(/(\d{3}(?!$))/g, '$1,')
        .split('').reverse().join('');
    return '$' + dollars + '.' + cents;
});

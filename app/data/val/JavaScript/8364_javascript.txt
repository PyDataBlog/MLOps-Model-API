// Fuzzy utility

'use strict';

var filter = require('array-filter');

var fuzzy = function(items, key) {
  return function (query) {
    query = query.toLowerCase();

    var re = new RegExp(query, 'i');

    return filter(items, function(item) {
      return re.test(item[key]);
    });
  };
};

module.exports = fuzzy;

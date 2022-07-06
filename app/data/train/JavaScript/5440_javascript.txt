var expect = require('chai').expect;
var search = require('./');

describe('binary search', function() {

  it('should search 0 elements', function() {
    var arr = [];
    expect(search(arr, 4)).to.equal(-1);
  });

  it('should search 1 element not found', function() {
    var arr = [1];
    expect(search(arr, 4)).to.equal(-1);
  });

  it('should search 1 element found', function() {
    var arr = [1];
    expect(search(arr, 1)).to.equal(0);
  });

  it('should search odd', function() {
    var arr = [1, 2, 3, 4, 5, 6, 7];
    expect(search(arr, 4)).to.equal(3);
  });

  it('should search even', function() {
    var arr = [1, 2, 3, 4, 5, 6, 7, 8];
    expect(search(arr, 4)).to.equal(3);
  });

});

'use strict';

const expect = require('chai').expect;
const deepfind = require('../index');

const simpleFixture = {
  'key': 'value'
};

const complexFixture = {
  'key1': {
    'key': 'value1'
  },
  'key2': {
    'key': 'value2'
  },
  'key3': {
    'key': 'value3'
  }
};

describe('deepfind', () => {

  it('should throw an error when given no object', () => {
    expect(() => deepfind(null, 'key')).to.throw('deepfind must be supplied an object');
  });

  it('should throw an error when given no key', () => {
    expect(() => deepfind({}, null)).to.throw('deepfind must be supplied a key');
  });

  it('should return an empty array when no key is found', () => {
    expect(deepfind({}, 'key')).to.be.an.array;
    expect(deepfind({}, 'key')).to.be.empty;
  });

  it('should return an array when one value matches', () => {
    expect(deepfind(simpleFixture, 'key')).to.be.an.array;
    expect(deepfind(simpleFixture, 'key')).to.deep.equal(['value']);
  });

  it('should return an array when multiple values match', () => {
    expect(deepfind(complexFixture, 'key')).to.be.an.array;
    expect(deepfind(complexFixture, 'key')).to.deep.equal(['value1', 'value2', 'value3'])
  });

});

var parentchild = require('../'),
  assert = require('assert');

assert.deepEqual(parentchild('foo'), []);
assert.deepEqual(parentchild({}), []);
assert.deepEqual(parentchild([1, 2, 3]), [['inarray', undefined, 1],
  ['inarray', undefined, 2],
  ['inarray', undefined, 3]]);
assert.deepEqual(parentchild({
  a: {
    b: 'foo'
  },
  c: ['a', 'b']
}), [['child', undefined, 'a'],
    ['child', 'a', 'b'],
    ['value', 'b', 'foo'],
    ['child', undefined, 'c'],
    ['inarray', 'c', 'a'],
    ['inarray', 'c', 'b']]);
assert.deepEqual(parentchild({
  a: {
    b: 'foo'
  },
  c: {
    f: 'baz',
    d: {
      e: 'bar'
    },
    g: 'darp',
    h: {
      i: 'derp',
      o: {
        p: {
          q: 'qak'
        },
        r: 'rez'
      }
    },
    j: 'gar'
  },
  k: {
    l: 'one',
    m: 'two',
    n: 'three',
    s: [1, 2, 3]
  }
}), [['child', undefined, 'a'],
  ['child', 'a', 'b'],
  ['value', 'b', 'foo'],
  ['child', undefined, 'c'],
  ['child', 'c', 'f'],
  ['value', 'f', 'baz'],
  ['child', 'c', 'd'],
  ['child', 'd', 'e'],
  ['value', 'e', 'bar'],
  ['child', 'c', 'g'],
  ['value', 'g', 'darp'],
  ['child', 'c', 'h'],
  ['child', 'h', 'i'],
  ['value', 'i', 'derp'],
  ['child', 'h', 'o'],
  ['child', 'o', 'p'],
  ['child', 'p', 'q'],
  ['value', 'q', 'qak'],
  ['child', 'o', 'r'],
  ['value', 'r', 'rez'],
  ['child', 'c', 'j'],
  ['value', 'j', 'gar'],
  ['child', undefined, 'k'],
  ['child', 'k', 'l'],
  ['value', 'l', 'one'],
  ['child', 'k', 'm'],
  ['value', 'm', 'two'],
  ['child', 'k', 'n'],
  ['value', 'n', 'three'],
  ['child', 'k', 's'],
  ['inarray', 's', 1],
  ['inarray', 's', 2],
  ['inarray', 's', 3]]);

console.log('ok');

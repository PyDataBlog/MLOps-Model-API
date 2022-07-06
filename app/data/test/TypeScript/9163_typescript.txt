
import * as test from 'tape';

import { toNumber } from '../../src/to';

test('toNumber:numbers', (t) => {
	t.equal(toNumber(0), 0);
	t.equal(toNumber(-0), 0);
	t.equal(toNumber(1), 1);
	t.equal(toNumber(-1), -1);
	t.equal(toNumber(1), 1);
	t.equal(toNumber(1.5), 1.5);
	t.equal(toNumber(-1.5), -1.5);
	t.end();
});

test('toNumber:strings', (t) => {
	t.equal(toNumber(''), 0);
	t.equal(toNumber('0'), 0);
	t.equal(toNumber('-0'), 0);
	t.equal(toNumber('1'), 1);
	t.equal(toNumber('-1'), -1);
	t.equal(toNumber('1.5'), 1.5);
	t.equal(toNumber('-1.5'), -1.5);
	t.end();
});

test('toNumber:Infinity', (t) => {
	t.equal(toNumber(Infinity), 0);
	t.equal(toNumber(Infinity, false), Infinity);
	t.equal(toNumber(-Infinity), 0);
	t.equal(toNumber(-Infinity, false), -Infinity);
	t.end();
});

test('toNumber:other', (t) => {
	t.equal(toNumber([]), 0);
	t.equal(toNumber({}), 0);
	t.equal(toNumber(null), 0);
	t.equal(toNumber(undefined), 0);
	t.equal(toNumber(new Error('error')), 0);
	t.end();
});

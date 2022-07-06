import test from 'ava';
import fn from './';

test('to decimal', t => {
	t.true(fn(65) === 0.65);
	t.true(fn(1234.5) === 12.345);
	t.true(fn(0.1) === 0.001);
	t.true(fn(12.1245, {digits: 2}) === 0.12);
	t.true(fn(6158.4256, {digits: 5}) === 61.58426);
	t.true(fn(1234.5, {digits: 0}) === 12);
	t.end();
});

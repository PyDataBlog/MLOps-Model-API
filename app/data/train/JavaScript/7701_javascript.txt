'use strict';

/**
 * Rounds a number to decimal places
 *
 * @param {number} number to be rounded
 * @param {integer} the number of place to round to
 * @param {String} the rounding method to be used
 * @returns {number} the number rounded to places
 */

function roundto(value, places, roundMethod) {
	var rtn = 0;
	var factorial = Math.pow(10, places);

	roundMethod = typeof roundMethod !== 'undefined' ? roundMethod : 'round';

	switch( roundMethod ) {
		case 'floor':
		case 'int':
			rtn = Math.floor( value * factorial );
			break;
		case 'ceiling':
			rtn = Math.ceil( value * factorial );
			break;
		default:
			rtn = Math.round( value * factorial );
			break;
	}

	// Divide number by factorial to get decimal places
	rtn = rtn / factorial;

	return rtn;
}

exports = module.exports = roundto;
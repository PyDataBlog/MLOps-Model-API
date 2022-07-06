function isPrime(n) {
	if (n === 2) {
		return true;
	}
    if (n % 2 === 0 || n === 1) {
        return false;
    }
    for (var i = 3; i <= Math.sqrt(n); i += 2) {
        if (n % i === 0) {
            return false;
        }
    }
    return true;
}

function nthPrimeNumber(n) {
	if (n <= 0) {
		throw new Error('Must be an integer >= 1');
	}
	var numPrime = 1;
	var i = 1;
	if (n === 1) {
		return 2;
	}
	while (numPrime < n) {
		i += 2;
		if (isPrime(i)) {
			numPrime++;
		}
	}
	return i;
}
console.log(nthPrimeNumber(10001));

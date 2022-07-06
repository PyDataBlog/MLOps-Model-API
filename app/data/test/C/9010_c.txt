/*
Fibonacci number generation

The program takes an input N from the command line
It uses a non-recursive technique to generate the value of N
Checks for conditions mentioned in the coding challenge.

*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Checks if a number is prime
int isPrime(long long n){
	if(n < 2){
		return 0;
	} else if(n == 2){
		return 1;
	} else if(n % 2){
		return 0;
	} else{
		for(long long i = 3; i < n; i += 2){
			if(n % i == 0){
				return 0;
			}
		}
	}

	return 1;
}

// Generate N fibonacci numbers
void fibonacci(unsigned long long n){
	long long a = 0, b = 1, c = 0;
	printf("%llu\n", a);
	printf("%llu\n", b);

	for(long long i = 2; i < (n + 1); i++){
		c = a + b;
		a = b;
		b = c;

		if(c % 3 == 0 && c % 5 == 0){
			printf("FizzBuzz\n");
		} else if(c % 3 == 0){
			printf("Buzz\n");
		} else if(c % 5 == 0){
			printf("Fizz\n");
		} else if(isPrime(c) == 1){
			printf("BuzzFizz\n");
		} else{
			printf("%llu\n", c);
		}
	}
}

int main(int argc, char *argv[]){
	if(argc < 2){
		printf("Program expects the value of N");
		return -1;
	}
	long long n = strtoul(argv[1], NULL, 10);
	fibonacci(n);
	return 0;
}
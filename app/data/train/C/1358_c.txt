/*
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 *
 * Find the sum of all the primes below two million.
 */

#include <stdio.h>
#include <stdint.h>
#include "euler.h"

#define PROBLEM 10

int
main(int argc, char **argv)
{
    uint64_t sum = 0, number = 0;

    do {
        number++;
        if(is_prime(number)) {
            sum += number;
        }
    } while(number != 2000000);
    printf("%llu\n", sum);
    return(0);
}

/*
 *
 *  20150315-4.c
 *
 *
 *  Created by Sam Niemoeller on 3/28/15.
 *
 * Chapter 4, Problem 30
 *
 * Write a program that generates a random number
 * from the following set: 1, 4, 7, 10, 13, 16
 * The seed for the series is the computer's time.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main ()
{
    // Local Definitions
    int rand1;
    
    // Statements
    printf ("\nBeginning Random Number generation from the set 1, 4, 7, 10, 13, 16....\n\n");
    
    srand (time(NULL));
    rand1 = rand ();
    rand1 = ((rand1 % 6) * 3) + 1;
    
    printf ("Random Number is: %d\n\n", rand1);
    
    return 0;
}
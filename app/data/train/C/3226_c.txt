#ifndef AI

#define AI

#include <stdlib.h>
#include <time.h>


int generateDecision() {
    /*
    A random number generator to pick the opponents moves
    */

    int randomDigit;

    randomDigit = (rand() % 5);

    return randomDigit;
}

#endif
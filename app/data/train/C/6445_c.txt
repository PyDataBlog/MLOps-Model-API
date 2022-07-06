#include <stdio.h>
#include <stdlib.h>

static const int MULTIPLE_MAX = 1000;

int main() {
   int i, sum;

   for (i = 0, sum = 0; i < MULTIPLE_MAX; ++i) {
      if (i % 3 == 0 || i % 5 == 0) {
         sum += i;
      }
   }

   fprintf(stdout, "The sum of multiples of 3 or 5 is: %d\n", sum);

   return EXIT_SUCCESS;
}

/**
 * Copyright (c) 2015 - Andrew C. Pacifico - All Rights Reserved.
 * @author Andrew C. Pacifico <andrewcpacifico@gmail.com>
 */
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    int n, i, j;

    scanf("%d", &n);

    for (i = 0; i < n; ++i) {
        for (j = 0; j < n - (i + 1); ++j) {
            printf(" ");
        }

        for (; j < n; ++j) {
            printf("#");
        }

        printf("\n");
    }

    return 0;
}

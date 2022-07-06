/**
 * Copyright (c) 2015 - Andrew C. Pacifico - All Rights Reserved.
 * @author Andrew C. Pacifico <andrewcpacifico@gmail.com>
 */
#include <iostream>
#include <stdlib.h>

using namespace std;

int main(int argc, char *argv[])
{
    int a, b, c;

    cin >> a >> b >> c;

    a = (a + b + abs(a-b))/2;

    cout << (a + c + abs(a-c))/2 << " eh o maior" << endl;

    return 0;
}

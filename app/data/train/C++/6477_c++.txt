// Lab 6.8 - Student Generated Code Assignments
//
// Option 2: Write a program that will input the number of wins and losses that
// a baseball team acquired during a complete season. The wins should be
// input in a parameter-less value returning function that returns the wins to
// the main function. A similar function should do the same thing for the
// losses. A third value returning function calculates the percentage of wins.
// It receives the wins and losses as parameters and returns the percentage
// (float) to the main program which then prints the result. The percentage
// should be printed as a percent to two decimal places.
//
//
// written by Walter B. Vaughan for CSC 134, section 200, Fall 2014
//  at Catawba Valley Community College

#include <iostream>
#include <iomanip>

unsigned getWins();
unsigned getLosses();
float calcPercentage( unsigned, unsigned );

int main() {
    std::cout << std::fixed << std::setprecision(2)
              << "The percentage of wins is "
              << calcPercentage( getLosses(), getWins() ) << "%\n";

    return 0;
}

unsigned getWins() {
    unsigned w;
    std::cout << "Please input the number of wins\n";
    std::cin >> w;
    return w;
}

unsigned getLosses() {
    unsigned l;
    std::cout << "Please input the number of losses\n";
    std::cin >> l;
    return l;
}

float calcPercentage( unsigned l, unsigned w ) {
    return ( static_cast<float>(w) / (w+l) ) * 100.0;
}

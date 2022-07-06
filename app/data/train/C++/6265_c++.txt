#include <iostream>
#include <cstdlib>

void get_grade(int g)
{
   std::cout << g << std::endl;
 
   if (g >= 90 && g <= 100) {
       std::cout << "A";
   } else if (g >= 80 && g <= 89) {
       std::cout << "B";
   } else if (g >= 70 && g <= 79) {
       std::cout << "C";
   } else if (g >= 60 && g <= 69) {
       std::cout << "D";
   } else if (g >= 0  && g <= 59) {
       std::cout << "F";
   } else {
       std::cout << "Please put a number.";
   }
   std::cout << std::endl;
}

int main(int argc, char** argv)
{
    std::cout << "what is your score?" << std::endl;
    get_grade(rand() % 100);
    return 0;
}


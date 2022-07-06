#include <stdio.h>

int main() {
  int current_char, previous_char;

  printf("Input text below, multiple spaces will be escaped:\n");

  previous_char = -1;

  while((current_char = getchar()) != EOF) {
    if (!(current_char == ' ' && previous_char == ' ')) {
      putchar(current_char);
    }

    previous_char = current_char;
  }
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int main(){

char text[40], text2[40];

printf("necesito una buena palabra:  ");
gets(text);

printf("voy a necesitar otra palabra, si es la misma sorpresa... \n");
    gets(text2);

if (strcmp(text, text2) ==0)
    printf("Correcto!! son iguales enhorabuena \n");

else
    printf("pues nada son distintas, no haces caso... \n" );




return EXIT_SUCCESS;


}

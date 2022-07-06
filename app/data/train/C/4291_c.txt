#include <stdio.h>

int main(void){
  int ni, nf, dez, cen, uni;

  printf("Digite o número: ");
  scanf("%d", &ni);

  cen = ni / 100;
  dez = (ni / 10) % 10;
  uni = ni % 10;
  nf = uni * 100 + dez * 10 + cen;
  printf("O número invertido é %d\n", nf);

  return 0;
}

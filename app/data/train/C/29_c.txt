// make a linked list that has one member
// then make 5 nodes - with each node having 1,2,3,4,5 as data
// then print them out
// then work out how to reverse the list by only changing the pointers
// then print again

#include <stdio.h>
#include <stdlib.h> // Including this header to use malloc

struct node {
  int num;
  struct node *next;
};

struct node *head = NULL;
struct node *p = NULL;

void insert(int num) {
  struct node *point = (struct node*) malloc(sizeof(struct node));

  point->num = num;
  point->next = NULL;

  if(head==NULL) {
    head = point;
    head->next = point;
    return;
  }
  p = head;

  while(p->next != head){
    p = p->next;
  }

  p->next = point;
  point->next = head;
}

void printNum() {
  struct node *pntr = head;

  printf("\nhead:");

  while(pntr->next != head) {
    printf(" %d ", pntr->num);
    pntr = pntr->next;
  }
  printf(" %d ", pntr->num);
  printf("\n");
}
int main() {
  insert(1);
  insert(2);
  insert(3);
  insert(4);
  insert(5);

  printNum();
}

#include<stdio.h>
void heap_sort(int[], int);
void build_max_heap(int[], int);
void max_heapify(int[],int,int);
void swap(int*, int*);
void display(int[],int);

int main(){
	int n = 11;
	int a[] = {55,3,2,5,77,44,65,53,88,31,9};
	display(a,n);
	heap_sort(a,n);
	display(a,n);
	return 0;
}

void heap_sort(int a[], int n){
	int i;
	int heap_size = n;
	build_max_heap(a,n);
	for(i = n-1; i > 0; i--){
		swap(&a[0], &a[i]);
		heap_size--;
		max_heapify(a,0,heap_size);
	}
}

void build_max_heap(int a[], int n){
	int i;
	int loop = (n-1)/2;
	for(i = loop; i >= 0; i--){
		max_heapify(a,i,n);
	}
}

void max_heapify(int a[], int i, int heap_size){
	int left = 2*i + 1;
	int right = 2*i + 2;
	int largest;
	if(left < heap_size && a[left] > a[i]){
		largest = left;
	}
	else{
		largest = i;
	}
	if((right < heap_size) && (a[right] > a[largest])){
		largest = right;
	}
	if(largest != i){
		swap(&a[largest], &a[i]);
		max_heapify(a, largest, heap_size);
	}
}

void swap(int *a,int *b){
	int temp;
	temp = *a;
	*a = *b;
	*b = temp;
}

void display(int a[],int n){
	int i;
	printf("\n");
	for(i = 0;i<n;i++){
		printf("%d\t",a[i]);
	}
}

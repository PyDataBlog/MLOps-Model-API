#include <iostream>
#include <fstream>

using namespace std;

int main(){

	ifstream f("problema_vectori_pr1.in");
	ofstream g("problema_vectori_pr1.out");

	int n;
	f >> n;
	int arr[n];
	for(int i=0;i<n;i++){
		f >> arr[i];
	}
	int x=3,max=0,premii=0;
	while(x){
		max=0;
		for(int i=0;i<n;i++){
			if(arr[i]>max)
				max=arr[i];
		}
		for(int i=0;i<n;i++){
			if(arr[i]==max){
				premii++;
				arr[i]=-11;
			}
		}
		x--;
	}
	g << premii;
}

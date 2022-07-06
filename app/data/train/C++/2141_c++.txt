#include <iostream>
#include <string>
#include <algorithm>
#include <fstream>
#define IN cin

using namespace std;

//bool sort_logic(int i, int j);
//void pr(int data[], int length);

// EOF Termination
int main(){
	
	int __initial_input, max_index, rear;
	
	int register i;
	
	//ifstream input;
	//input.open("input.txt");
	
	for(IN >> __initial_input; __initial_input != 0; IN >> __initial_input){
		
		int data[__initial_input][3];
		
		for(i = 0; i < __initial_input; i++){
			
			IN >> data[i][0] >> data[i][1] >> data[i][2];
			
			}
		
		for(i = 0, max_index = 0; i < __initial_input; i++){
			
			if(data[i][2] > data[max_index][2]){
				
				max_index = i;
				
				}
			
			}
		
		// Cheacking if has any equeal
		int max_volumes[__initial_input];
		rear = 0;
		for(i = 0; i < __initial_input; i++){
			
			if(data[max_index][2] == data[i][2]){
				
				max_volumes[rear] = i;
				rear++;
				
				}
			
			}
		
		// Piciking up the optimal result
		for(i = 0, max_index = max_volumes[i]; i < rear; i++){
			
			if( (data[max_volumes[i]][0] * data[max_volumes[i]][1] * data[max_volumes[i]][2]) > (data[max_index][0] * data[max_index][1] * data[max_index][2]) ){
				
				max_index = max_volumes[i];
				
				}
			
			}
		
		cout << (data[max_index][0] * data[max_index][1] * data[max_index][2]) << endl;
		
		}
	
	//input.close();
	
	return 0;
	
	}

// Input Termination
/*int main(){
	
	int __initial_input;
	
	for(cin >> __initial_input; __initial_input != 0; cin >> __initial_input){
		
		cout << "Initail Input ::" << __initial_input << endl;
		
		}
	
	return 0;
	
	}*/

// Case Number
/*int main(){
	
	int __initial_input, __case_number;
	
	//ifstream input;
	//input.open("input.txt");
	
	for(IN >> __initial_input, __case_number = 1; __case_number <= __initial_input; __case_number++){
		
		cout << "Case #" << __case_number << endl;
		
		}
	
	//input.close();
	return 0;
	
	}


bool sort_logic(int i, int j){
	
	return i > j;
	
	}

void pr(int data[], int length){
	
	int i;
	for(i = 0; i< length; i++) cout << data[i] << " ";
	cout << endl;
	
	}

*/

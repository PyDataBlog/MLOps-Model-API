// Source Code Begins Here

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<time.h>

void Initial(char board[3][3]){ // Function to Initialize board with value 1,2,3....9
	int i,j,k = 1;
	for(i=0;i<3;i++){
		for(j=0;j<3;j++){
			board[i][j] = '0'+k;
			k++;
		}
	}
}

void View(char board[3][3]){ // Function to View to board
	for(int i=0;i<3;i++){
		for(int j=0;j<3;j++)
			printf("%c\t",board[i][j]);
		printf("\n\n");
	}
}

bool WinningCheck(char board[3][3],char mark){ // Checking Winning Conditions
	if(board[0][0]==mark && board[0][1]==mark && board[0][2]==mark) return true;
	else if(board[1][0]==mark && board[1][1]==mark && board[1][2]==mark) return true;
	else if(board[2][0]==mark && board[2][1]==mark && board[2][2]==mark) return true;
	else if(board[0][0]==mark && board[1][0]==mark && board[2][0]==mark) return true;
	else if(board[0][1]==mark && board[1][1]==mark && board[2][1]==mark) return true;
	else if(board[0][2]==mark && board[1][2]==mark && board[2][2]==mark) return true;
	else if(board[0][0]==mark && board[1][1]==mark && board[2][2]==mark) return true;
	else if(board[0][2]==mark && board[1][1]==mark && board[2][0]==mark) return true;
	return false;
}

int TieCheck(char board[3][3]){ // Function to Check Tie Case
	int i,j,k = 1;
	for(i=0;i<3;i++){
		for(j=0;j<3;j++){
			if(board[i][j]=='0'+k) return 1;
			k++;
		}
	}
	return 0;
}

bool AssignValue(char board[3][3],int value,char mark){ // Function to checking value assigning validity
	if(value==1 && board[0][0]=='1') { board[0][0] = mark; return false; }
	else if(value==2 && board[0][1]=='2') { board[0][1] = mark; return false; }
	else if(value==3 && board[0][2]=='3') { board[0][2] = mark; return false; }
	else if(value==4 && board[1][0]=='4') { board[1][0] = mark; return false; }
	else if(value==5 && board[1][1]=='5') { board[1][1] = mark; return false; }
	else if(value==6 && board[1][2]=='6') { board[1][2] = mark; return false; }
	else if(value==7 && board[2][0]=='7') { board[2][0] = mark; return false; }
	else if(value==8 && board[2][1]=='8') { board[2][1] = mark; return false; }
	else if(value==9 && board[2][2]=='9') { board[2][2] = mark; return false; }
	return true;
}

int Play(int run,char board[3][3],char pname[]){ // TIK TOK TOE Begins
	int ch;
	bool ok = true;
	run = TieCheck(board);
	if(!run){
		printf("Match Tied\n\n");
		printf("Press Any Key to Continue...\n");
		getchar();
	}
	if(run){
		system("clear");
		View(board);
		while(ok){
			printf("%s , Enter Your Choice ( 1 - 9 ) : ",pname);
			scanf("%d",&ch);
			getchar();
			ok = AssignValue(board,ch,'X');
			if(ok){ printf("Wrong Input. PLease Enter a valid one\n"); }
		}
		if(WinningCheck(board,'X')){
			system("clear");
			View(board);
			printf("Congrats %s , You Win The Match :D ^_^\n\n",pname);
			printf("Press Any Key to Continue...\n");
			getchar();
			return 0;
		}
		run = TieCheck(board);
		if(!run){
			printf("Match Tied\n\n");
			printf("Press Any Key to Continue...\n");
			getchar();
		}
		ok = true;
		system("clear");
		View(board);
		while(ok && run){
			ch = 1 + rand()%9;
			ok = AssignValue(board,ch,'O');
		}
		if(WinningCheck(board,'O')){
			system("clear");
			View(board);
			printf("Sorry %s , You Loss The Match :'(\n\n",pname);
			printf("Press Any Key to Continue...\n");
			getchar();
			return 0;
		}
		Play(run,board,pname);
	}
}

int main(){ // Main Function
	srand(time(NULL));
	char pname[30];
	char board[3][3];
	int choice;
	while(1){
		Initial(board);
		system("clear");
		printf("Welcome 2 TIK TOK TOE\n\n");
		printf("Please Choice A Option +++\n\n");
		printf("1. Play\n2. Exit\n\n");
		scanf("%d",&choice);
		getchar();
		if(choice==1){
			printf("Please Enter Your Name : "); // Getting Player Name
			gets(pname);
			Play(1,board,pname); // Game begining call
		}
		else if(choice==2) break;
		else { printf("Wrong Choice. Enter a valid one please.\n"); getchar(); system("clear"); }
	}
	return 0;
}

// Source Code Ends Here

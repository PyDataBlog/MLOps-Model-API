#include <stdio.h>

int findbig(const int*,int);

int main(int argc, char const *argv[])
{
	int lines;
	char s[1010]={'\0'},*pstr=s;
	scanf("%d",&lines);
	getchar();
	while(lines--){
		int count[26]={0};
		gets(s);
		pstr=s;
		while(*pstr)
			count[*pstr++-'a']++;
		printf("%c\n",findbig(count,26)+'a');
	}
	return 0;
}

int findbig(const int* p,int n)
{
	int big=0,index=-1;
	for(int i=0;i<n;i++)
		if(*(p+i)>big){
			big=*(p+i);
			index=i;
		}
	return index;
}
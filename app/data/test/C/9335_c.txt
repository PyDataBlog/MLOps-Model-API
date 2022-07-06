#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <stdbool.h>

int checkString(char *s,int i,int j,int len){
    char X = 'a'+i;
    char Y = 'a'+j;
    int top =-1;
    char *stack = (char *)malloc(sizeof(char)*len);
    
    for(int i = 0; i < len; i++){
        if(top==-1 &&(X==s[i]||Y==s[i])){
            top++;
            stack[top]=s[i];
        }else{
            if(s[i]==X && stack[top]==Y){
                top++;
                stack[top]=s[i];
            }else if(s[i]==Y && stack[top]==X){
                top++;
                stack[top]=s[i];                    
            }else if(s[i]==X && stack[top]==X || (s[i]==Y && stack[top]==Y)){
                top = -1;
                break;
            }
        }
    }
    return top;
}

int main(){
    int len; 
    int i,j;
    scanf("%d",&len);
    char* s = (char *)malloc(512000 * sizeof(char));
    scanf("%s",s);
    
    int max = 0;
    int maxI,maxJ;
   
    for(i=0;i<26;i++){
        int flag = 1;
        for(j=i+1;j<26;j++){
            int ans = checkString(s,i,j,len);
            if(ans > max){
                max = ans;
                maxI = i;
                maxJ = j;
            }
        }
    }
    
    if(max !=0){
        printf("%d",max+1);    
    }else{
        printf("0");
    }
    
    
    return 0;
}

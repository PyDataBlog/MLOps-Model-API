// EX.1 - READ A TEXT FILE CHAR BY CHAR
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main()
{
    FILE *bin;   //declare a file pointer variable
    FILE *numfile;
    char s[20] = "1234";
    int ch;
    int i;
    char line[50];
    
    numfile = fopen("numbers.txt","r");
    bin = fopen("numbers.txt","wb");  
    //open the file, text reading mode
    
    if (numfile == NULL) 
    { //test if everything was ok
        printf("Cannot open file.\n");
        exit(1);
    } // Error checking
      
    while(fgets(line,50,numfile) != NULL) 
    {
        i = atoi(s);
        fwrite(&i,sizeof(int),1,bin);  
    }
    
    getchar();
    fclose(bin);
    fclose(numfile); //close the files
    return 0;
} // end main()

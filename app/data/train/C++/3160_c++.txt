#include <iostream>

using namespace std;

main(){
    
    char palavra[] = ".";
    
    cin.getline(palavra, 10, '\n'); // irá ler apenas 10 caracteres e vai ser delimitado até encontrar a quebra de linha
    
    cout << endl << palavra << endl;
    
    
}

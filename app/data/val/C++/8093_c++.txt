#include <iostream>
#include <math.h>
using namespace std;
int main()
{
    int temp,b,n,a;
    float aa;
    cin >> b >>n;
    while(b!=0&&n!=0){
        aa=pow(b,1.0/n);
        a=floor(aa);
        if((pow(a+1,n)-b)>(b-pow(a,n))){
            cout << a <<endl;
        }
        else{
            cout << a+1<<endl;
        }
        cin >> b >>n;
    }
    return 0;
}
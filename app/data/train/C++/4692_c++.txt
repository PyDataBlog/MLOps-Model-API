#include <iostream>
using namespace std;

int gcd(int num1,int num2);

int main()
{
int x=40;
int y =150;
int result =gcd(x,y);
cout << "the gcd of " << x << " and " << y<<" is "<< result;

}

int gcd(int num1, int num2)
{
	int m,i;
	if(num1>num2)
	{
	   m=num2;
	}
	else
	{
	   m=num1;
	}

	for(i=m;i>=1;i--)
	{
	    if(num1%i==0 && num2%i==0)
		{
			m=i;
			break;
		}
	}

	return m;


}

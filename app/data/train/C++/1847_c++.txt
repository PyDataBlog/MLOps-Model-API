//BE NAME DOOST
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <cmath>
#include <set>
#include <iostream>
using namespace std;


int main()
{
	string a,b;
	cin>>a>>b;
	set <char> d;
	for(int i=0;i<a.size();i++)
		d.insert(a[i]);
	int B=0,C=0;
	for(int i=0;i<b.size();i++)
	{
		if(a[i]==b[i])
			B++;
		else
		{
			set <char> tmp=d;
			int s=tmp.size();
			tmp.insert(b[i]);
			if(s==tmp.size())
				C++;
		}
	}
	cout<<B<<" "<<C;
}

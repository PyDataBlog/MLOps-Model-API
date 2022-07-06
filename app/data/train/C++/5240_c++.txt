#include<iostream>
#include<cstdio>
#include<cmath>
#include<cctype>
#include<cstring>
#include<cstdlib>
#include<string>
#include<vector>
#include<algorithm>
#include<map>
#include<iomanip>

using namespace std;

int main()
{
	int n,i,N[1000],temp,cnt;
	bool flag;

	while(cin >> n)
	{
		cnt=0;

		for(i=0;i<n;i++)
		{
			cin >> N[i]; 
		}

		while(1)
		{
			flag=true;

			for(i=1;i<n;i++)
			{
				if(N[i]<N[i-1])
				{
					temp=N[i];
					N[i]=N[i-1];
					N[i-1]=temp;
					cnt++;
					flag=false;
				}
			}

			if(flag)
			{
				break;
			}
		}

		cout << "Minimum exchange operations : " << cnt <<endl;
	}
	return 0;
}
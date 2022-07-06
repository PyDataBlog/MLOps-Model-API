# include<cstdio>
# include<cmath>
# include<algorithm>
using namespace std;
int main()
{
	int t,n,m;
	double ans;
	scanf("%d",&t);
	while(t--)
	{
		ans=0;
		scanf("%d",&n);
		m=n;
		while(n)
		{
			ans += (1.0/n);
			n--;
		}
		ans*=m;
		printf("%.2lf\n",ans);
	}
	return 0;
}
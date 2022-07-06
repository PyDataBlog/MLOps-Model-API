/** Author: Sharmin Akter **/
/** Created at: 4/30/2012 12:07:26 AM **/

#include<stdio.h>

int main()
{
    int i,j,k,p,m,n,t;

    while(scanf("%d",&t)==1)
    {
        for(i=1;i<=t;i++)
        {
            scanf("%d",&p);
            if(p==2||p==3||p==5||p==7||p==13||p==17)
                printf("Yes\n");
            else
                printf("No\n");
            getchar();
        }
    }
return 0;
}

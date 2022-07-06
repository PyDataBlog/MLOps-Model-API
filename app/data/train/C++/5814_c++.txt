#include <bits/stdc++.h>

using namespace std;

int n,m;
int r;

int main()
{
    scanf("%d %d",&n,&m);

    while(n>0)
    {
        n--;
        r++;

        if(r%m==0)
            n++;
    }

    printf("%d",r);
    return 0;
}
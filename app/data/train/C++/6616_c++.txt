#include <stdio.h>

double f(int n)
{
    int i = 0;
    int k = 1;
    double r = 0.000;
    double fi = 0.000;

    for (i = 1; i < n+ 1; ++i)
    {
        fi = i;
        r += k*1.000/fi;
        k = -k;
    }

    return r;
}

int main()
{
    int n;
    int m;

    scanf("%d",&n);

    while(n-- >0 )
    {
        scanf("%d",&m);
        printf("%.2lf\n",f(m));
    }

    return 0;
}

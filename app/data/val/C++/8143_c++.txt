#include <iostream>
#include <string.h>

using namespace std;

int p[10001];

int cut_rod(int n)
{
    int q;
    if (n == 0)
        return 0;
    q = 0;
    for (int i=1; i<=n; i++)
    {
        q = max(q, p[i] + cut_rod(n-i));
    }
    return q;
}

int main()
{
    int n;
    memset(p, 0, sizeof(p));

    p[0] = 0;
    p[1] = 1;
    p[2] = 5;
    p[3] = 8;
    p[4] = 9;
    p[5] = 10;
    p[6] = 17;
    p[7] = 17;
    p[8] = 20;
    p[9] = 24;
    p[10] = 30;

    cin >> n;
    cout << cut_rod(n);
}

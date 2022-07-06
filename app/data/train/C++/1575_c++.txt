#include <cstdio>
#include <algorithm>
using namespace std;

const int maxn = 111;
int arr[maxn];

int main()
{
    //freopen("in.txt", "r", stdin);
    int n, m;
    while(2 == scanf("%d%d", &n, &m) && !(n==0 && m==0)) {
        for(int i = 0; i < n; ++i)
            scanf("%d", &arr[i]);
        arr[n] = m;
        sort(arr, arr+n+1);
        printf("%d", arr[0]);
        for(int i = 1; i < n+1; ++i)
            printf(" %d", arr[i]);
        printf("\n");
    }
    return 0;
}

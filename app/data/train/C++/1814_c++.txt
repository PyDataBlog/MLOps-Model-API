#include <iostream>
#include <cstring>
#define  MAXN 40

struct Node
{
    int volume;
    int weight;
} ;

using namespace std;

Node test[MAXN];
int Max[MAXN];
int limit, number;

int dp( int );

int main( void )
{
    cout << "Please iuput number: ";
    cin >> number;
    cout << "Please input limit volume: ";
    cin >> limit;
    cout << "Please input volume and weight:" << endl;
    for ( int i = 0; i < number; i++ ){
        cin >> test[i].volume >> test[i].weight;
    }
    memset( Max, -1, sizeof( Max ) );
    cout << dp( limit ) << endl;

    return 0;
}
int dp( int n )
{
    int &ans = Max[n];
    if ( ans != -1 ){
        return ans;
    }
    ans = 0;
    for ( int i = 1; i <= n; i++ ){
        for ( int j = 0; j < number; j++ ){
            if ( i >= test[j].volume ){
                int temp = dp( i - test[j].volume ) + test[j].weight;
                ans = ans > temp ? ans : temp;
            }
        }
    }
    return ans;
}

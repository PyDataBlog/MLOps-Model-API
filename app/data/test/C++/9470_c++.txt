#include <bits/stdc++.h>
#define CBIT(var, pos) ((var) & (1 << (pos)))
using namespace std;

int main() {
    ios::sync_with_stdio(0);
    cin.tie(0);

    long T, F, X;
    cin >> T;

    while (T--) {
        cin >> F >> X;
        bool flag = false;

        for (unsigned long long i = 0; i < (1 << F); i++) {
            unsigned long long m = 1;
            for (unsigned long long a = 0; a < F; a++) {
                if (CBIT(i, a)) {
                    m *= (a+1);
                }
            }
            if (m == X) {
                flag = true;
                break;
            }
        }
    
        if (flag) cout << "yes\n";
        else cout << "no\n";
    }
   
    return 0;
}
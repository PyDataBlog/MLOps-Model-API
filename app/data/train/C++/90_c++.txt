#include <bits/stdc++.h>

using namespace std;

int count_consecutive(string &s, int n, int k, char x) {
    int mx_count = 0;
    int x_count = 0;
    int curr_count = 0;
    int l = 0;
    int r = 0;

    while (r < n) {

        if (x_count <= k) {
            if (s[r] == x)
                x_count++;
            r++;
            curr_count++;
            if (s[r-1] != x) mx_count = max(mx_count, curr_count);
        } else {
            if (s[l] == x) {
                x_count--;
            }
            l++;
            curr_count--;
        }

    }

    if (s[s.size()-1] == x && x_count) mx_count++;

    return mx_count;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, k;
    string s;

    cin >> n >> k;
    cin >> s;

    cout << max(count_consecutive(s, n, k, 'b'), count_consecutive(s, n, k, 'a')) << endl;

    return 0;
}


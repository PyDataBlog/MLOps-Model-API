#include <bits/stdc++.h>
using namespace std;

using ull = unsigned long long;

ull arr[int(1e5)+5];
int n, q, x;

int bs(int key) {
	int l = 0, r = n-1, mid, ans=-1;

	while (l <= r) {
		mid = l + (r - l + 1)/2;

		if (arr[mid] == key) {
			r = mid - 1;
			ans = mid;
		}
		else if (arr[mid] < key) l = mid + 1;
		else r = mid - 1;
	}

	return ans;
}

int main() {
	ios::sync_with_stdio(0);
	cin.tie(0);

	cin >> n >> q;

	for (int i = 0; i < n; ++i) cin >> x, arr[i] = x + INT_MAX;

	for (int i = 0; i < q; ++i) {
		cin >> x;
		cout << bs(x + INT_MAX) << '\n';
	}

	return 0;
}


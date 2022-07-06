#include <bits/stdc++.h>
using namespace std;

using ii = pair<int, int>;
using vi = vector<int>;
using vii = vector<ii>;

int n, q;
vi vec;
vii pairs;
bool cnt[5005];

int max_length_covered() {
	memset(cnt, 0, sizeof cnt);
	int c = 0;
	for (int i : vec) {
		for (int p = pairs[i].first; p <= pairs[i].second; ++p) {
			if (!cnt[p]) ++c;
			cnt[p] = 1;
		}
	}
	return c;
}

int f(int i, int taken) {
	if (i > q) return 0;
	if (taken == q-2) return max_length_covered();

	vec.emplace_back(i);
	int p = f(i+1, taken+1);
	vec.pop_back();

	int q = f(i+1, taken);
	return max(p, q);
}

int main() {
	ios::sync_with_stdio(0);
	cin.tie(0);

	cin >> n >> q;
	int l, r;
	for (int i = 0; i < q; ++i) {
		cin >> l >> r;
		pairs.emplace_back(l, r);
	}

	cout << f(0, 0) << endl;

	return 0;
}


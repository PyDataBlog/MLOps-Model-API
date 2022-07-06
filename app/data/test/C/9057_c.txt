#pragma once
#include<vector>
#include<string>
#include<cstring>
#include<climits>
#include<algorithm>
#include<cmath>
#include<stack> 
#include <numeric>
#include<unordered_map>
#include<unordered_set>
#include<sstream>
using namespace std;


class Solution {
public:
	int f[1000] = {0};
	int numTrees(int n) {
		if (n == 0 || n==1) return 1;
		if (f[n] != 0) {
			return f[n];
		}
		int ans = 0;
		for (int i = 0; i < n; i++) {
			ans += numTrees(i)*numTrees(n - 1 - i);
		}
		f[n] = ans;
		return ans;
	}
};
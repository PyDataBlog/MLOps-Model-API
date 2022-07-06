#include <iostream>
#include <string>

// time complexity O(1) - constant
// space complexity O(1) - constant

bool is_unique (std::string s) {
	if (s.length() > 128) return false;

	bool char_list[128] = {0};

	for (int i = 0; i < s.length(); i++) {
		if (char_list[s[i]])	return false;
		char_list[s[i]] = true;
	}
	return true;
}

int main () {
	std::cout << is_unique("ABCDEFGA") << std::endl;
}
package pangram

var isPangramTestCases = []struct {
	phrase   string
	expected bool
}{
	{"", false},
	{"The quick brown fox jumps over the lazy dog", true},
	{"a quick movement of the enemy will jeopardize five gunboats", false},
	{"the quick brown fish jumps over the lazy dog", false},
	{"the 1 quick brown fox jumps over the 2 lazy dogs", true},
	{"7h3 qu1ck brown fox jumps ov3r 7h3 lazy dog", false},
	{"\"Five quacking Zephyrs jolt my wax bed.\"", true},
	{"Victor jagt zwölf Boxkämpfer quer über den großen Sylter Deich.", true},
	{"Широкая электрификация южных губерний даст мощный толчок подъёму сельского хозяйства.", false},
}

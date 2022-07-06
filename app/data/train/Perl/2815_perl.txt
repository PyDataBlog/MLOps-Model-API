#!/usr/bin/perl -w

open PW,"< /etc/passwd";
while (<PW>) {
	chop;
	@vals = split(":");
	print "@vals\n";
	$key = shift(@vals);
	$hash{$key}=\@vals;
}

foreach (keys(%hash)) {
	print "$_ -> @{$hash{$_}}\n";
}

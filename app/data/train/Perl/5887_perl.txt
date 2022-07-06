#!/usr/bin/env perl

use strict;
use warnings;

open my $fh, '<', $ARGV[0];

for my $line ( <$fh> ) {
	my @parts = split /,/, $line;
	my $x     = $parts[0];
	my $n     = $parts[1];

	my $v = $n;
	while ( $v < $x ) {
		$v += $n;
	}

	print "$v\n";
}

close $fh;


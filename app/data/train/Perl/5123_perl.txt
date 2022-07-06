#!/usr/bin/perl
use Term::ANSIColor;

# Clean screen
if ($^O eq 'MSWin32') {
	system 'cls';
} else {
	system 'clear';
}

# Get dictionary output
$_ = `sdcv -n @ARGV`;
	s/(\d[.)])/\n$1/g;
	s/\n\n\n\n/\n\n/g;
	s/\&apos;/\`/g;
	s/\&quot/\"/g;
	s/\&gt/\>/g;
	s/\&lt/\</g;
	s/\&amp/ \& /g;
my @strings = split ( '\n', $_ );

# Lets colorize it
foreach (@strings) {
	my $printed = 0;
	# Match Dictionary and Article
	if ( m/-->.*/ ) {
		print color 'blue';
		print "$_\n";
		print color 'reset';
		$printed = 1;
	}
	if ( m/(.*)(\[.*?\])(.*)/ ) {
	    print $1;
		print color 'bold';
		print $2;
		$_ = $3;
		if ( m/(\s+\S+\.\s*)(.*)/ ) {
			print color 'green';
			print $1;
			print color 'reset';
			print $2;
		} else {
			print $3;
		}
		print "\n";
		$printed = 1;
	}
	if ( m/(.*)(\d[.)])(\s+\S+\.\s*)(.*)/ ) {
		print $1;
		my $point = $2;
		my $str = $3;
		my $rest = $4;
		$_ = $point;
		print color 'bold' if ( m/\d\./ ) ;
		print $point;
		print color 'green';
		print $str;
		print color 'reset';
		print "$rest\n";
		$printed = 1;
	}
	if ( m/(.*)(Syn :)(.*)/g ) {
		print "$1";
		print color 'CYAN';
		print "$2";
		print color 'reset';
		print "$3\n";
		$printed = 1;
	}
	if ( ! $printed ) {
		print "$_\n";
	}
}

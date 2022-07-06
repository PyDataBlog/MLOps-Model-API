#!/usr/bin/perl -w

$command = 1;

while ($command>0) {
	print "0) exit\n";
	print "1) convert integer to char\n";
	print "2) convert string to integers\n";
	print "3) ASCII table\n";
	print "Please choose:";

	$command = <>;

	if ($command==1) {
		# OPTION 1
		$val = <>;
		print "char value:".chr($val)."\n";
	} elsif ($command==2) {
		# OPTION 2
		$string = <>;
		while (length($string)>0) {
			print ord(substr($string,0,1))." ";
			$string = substr($string,1,length($string)-1);
		}
		print "\n";
	} elsif ($command==3) {
		# OPTION 3
		for ($i=32;$i<128;$i++) {
			print $i."\t".chr($i)."\t";
		}
		print "\n";
	}
}

print "bye";

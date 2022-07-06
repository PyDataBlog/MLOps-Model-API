#!/usr/bin/perl

use strict;
use warnings;


sub quadr_calc
{
	my($d,$x,$x1,$x2);
	my($a,$b,$c) = @_;
	if($a!=0)
	{
	$d=$b*$b-(4*$a*$c);
	if($d<0)
	{
		print "D<0: No solution";
	}
	elsif($d==0)
	{
		$x=(0-$b)/(2*$a);
		print "D=0: X1=X2=".$x;		
	}
	elsif($d>0)
	{
		$x1=((0-$b)+($d**0.5))/(2*$a);
		$x2=((0-$b)-($d**0.5))/(2*$a);
		print "D>0: X1=".$x1." X2=".$x2;
	}
	}
	else
	{
		$x=(0-$c)/$b;
		print"a=0: Not a qiadratic function! \nX=".$x;
	}
}	

print "This program takes three numbers (a, b and c) as coefficients\n";
print "of a quadratic equation, calculates its roots, and displays them\n";
print "on the screen for you.\n\n";
print "Please enter the value of a and press <ENTER>: ";
my $a = <STDIN>;
print "\n";
print "Please enter the value of b and press <ENTER>: ";
my $b = <STDIN>;
print "\n";
print "Please enter the value of c and press <ENTER>: ";
my $c = <STDIN>;
print "\n";


quadr_calc($a,$b,$c);
print "\n";

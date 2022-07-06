#!/usr/bin/perl -w

use 5.010;
use strict;

chomp( my $num1 = <STDIN> );
chomp( my $num2 = <STDIN> );

my $res = 0; 
my $error_msg = "wrong input, ";
$res = $num1 * $num2, $error_msg = "", if( &is_numeric($num1) && &is_numeric($num2) );
say $error_msg, "\$res is $res";

sub is_numeric{
    $_ = shift @_;
    1 if( /^-?\d{1,}\.?\d{0,}$/ );
}

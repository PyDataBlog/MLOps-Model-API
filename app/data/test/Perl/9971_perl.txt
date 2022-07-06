#!/usr/bin/perl -w
# Solgenomics@BTI
# Surya Saha DATE??
# Purpose: 


unless (@ARGV == ??){
	print "USAGE: $0 <??> \n";
	exit;
}

use strict;
use warnings;
use lib '/home/surya/bin/modules';
use SS;

my ($ifname,$rec,$i);

$ifname=$ARGV[0];
unless(open(IN,$ifname)){print "not able to open ".$ifname."\n\n";exit;}
unless(open(OUT,">$ifname.results")){print "not able to open ".$ifname."results\n\n";exit;}


close (IN);
close (OUT);


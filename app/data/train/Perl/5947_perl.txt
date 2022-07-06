#!/usr/bin/perl
use 5.010;
$elementcount=0;
$i =0;

given ( $elementcount ) {
        when ( 1 ) { $i=1; }
        when ( 2 ) { $i=2; }
        when ( 3 ) { $i=3; }
        when ( 4 ) { $i=4; }
        default {$i=0;}
}
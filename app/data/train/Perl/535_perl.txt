use strict;
use warnings;
use 5.018;

chomp(my @lines = <>);
foreach (@lines){
    if(/\./){
        say $_;
    }
} 

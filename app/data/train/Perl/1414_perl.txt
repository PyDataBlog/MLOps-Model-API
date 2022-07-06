use strict;
use warnings;
use 5.018;

while(<>){
    chomp;
    if(/(?<word>\b\w*a)(?<after>\w{1,5})*/){
        print "'word' contains '$+{word}' \n";
        if(defined $+{after}){
            print "and 'after' contains '$+{after}'\n" 
        }
    }else{
        print "No match: |$_| \n";
    }       
}

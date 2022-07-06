#!/usr/bin/perl

@dir = `find  documents/Gender_Queer_Essex_HS/ -type f`;

foreach(@dir)
{
    chomp;
    print '<p>';
    print "\n";
    print '<a href="{{ site.url }}';
    print "$_";
    print '">';
    print "$_";
    print '</a>';
    print "\n";
    print '</p>';
    print "\n";
    print "\n";
}



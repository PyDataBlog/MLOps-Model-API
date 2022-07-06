#!/usr/bin/perl

use warnings;
use strict;
use 5.010; #<- so much time wasted because of this
# For some reason when compiling given/when statements
# rather than the error being something like:
# > Warning: compilation will fail for < perl 5.01 
# the error is: 
# > syntax error at file.pl line x, near ") {"
# I hate perl so much, if I had some actual cojones 
# I'd learn a better scripting language.
#
# F$$K-$$$$$$$ [Redacted].

use Text::CSV;
use GD::Simple;

my @rows;

my $file11 =  $ARGV[0];

@rows = loadCSV($file11, @rows);

my $dpi = 300;

#all measurements in mm
my $dpmm = (1/25.4)*$dpi;

my $pagewidth = 222.25; #A4 width mm
my $pageheight = 298.45; #A4 height


my $marginleft = 35;
my $marginright = 35;
my $margintop = 20;
my $marginbottom = 20;

#I wish the image to take up 
my $width_aspercent = 1;# fraction of the page's width
# and 
my $height_aspercent = 0.2;# fraction of the page's height

my $boxed = 1;

my $internalmargin = 0.05;

#height of the 'lines'
my $barheights = 0.1;

my $drawwidths = 6;

my $pixwidth = int((($pagewidth-$marginleft-$marginright)*$width_aspercent)*$dpmm);
my $pixheight = int((($pageheight-$margintop-$marginright)*$height_aspercent)*$dpmm);
#print "Pixel width" . $pixwidth . "\n";
#print "Pixel height" . $pixheight . "\n";
#create a new image
$pixheight < 10000 or die("Image too big!");
$pixwidth < 10000 or die("Image too big!");


$barheights = int($barheights*$pixheight);


my $startx = int($internalmargin*$pixwidth);
my $starty = ($pixheight/2) - ($barheights/2);

my $rowcount = 0;
my $elementcount=0;
my $switch =0;

#
#Start drawing
#

my $img = GD::Simple->new($pixwidth,$pixheight);

if($boxed == 1){
    drawBorder($img, $pixwidth,$pixheight, $drawwidths);
}

foreach my $row (@rows) {
    foreach my $element (@$row) {
	if($elementcount == 0 && ($element eq '#0') == 1){
	    $switch++;
	}
	if($switch == 1){
# 	    given ($elementcount) {
# 		when(0) {print 'n';}
# 		when(1) {print 'n';}
# 		when(2) {print 'n';}
# 		when(3) {print 'n';}
# 		when(4) {print 'n';}
# 		when(5) {print 'n';}
# 		when(6) {print 'n';}
# 		when(7) {print 'n';}
# 		default { print 'n'; }
# 	    }
	}
	elsif($switch == 2){
	
	}
	
	$elementcount++;
    }
    $elementcount=0;
    $rowcount++;
}

#($img, $width, $heigth)
sub drawBorder{
    $_[0]->penSize($_[3],$_[3]);
    $_[0]->bgcolor(undef);
    $_[0]->fgcolor('black');
    $_[0]->rectangle(3,3,$_[1]-4,$_[2]-4);
}

#sub drawGeneLine{
    
  
#}

sub loadCSV{
    my $file2 = $_[0];
    my @rows1;
    my $csv = Text::CSV->new ({ binary => 1, eol => $/ });
    open my $io, "<", $file2 or die "$file2: $!";
    while (my $row = $csv->getline($io)){
        push @rows1, $row;
    }
    $csv->eof or $csv->error_diag();

    close $io
        or die "Failed to close $file2 ($!)";
    return @rows1;
}


print $img->png;
#!/usr/bin/perl -w
# Creation Date: Dec 5th PST 2007

use strict;
use warnings;

($#ARGV == 0) or die "Usage: $0 <plot_data>\n";
my $infile = shift;

# Some Gnuplot notes:
#set terminal postscript eps color "Helvetica" 22
#set output "average_degree.eps"

#1    +              7   (f) circle
#2    cross          8   (o) triangle
#3    *              9   (f) triangle
#4    (o) square    10   (o) inverse-triangle
#5    (f) square    11  (f) inverse-triangle
#6    (o) circle    12  (o) diamond
#13   (f) diamond

# type 'test' to see the colors and point types available
# lt is for color of the points: -1=black 1=red 2=grn 3=blue 4=purple 5=aqua 6=brn 7=orange 8=light-brn
# pt gives a particular point type: 1=diamond 2=+ 3=square 4=X 5=triangle 6=*
# postscipt: 1=+, 2=X, 3=*, 4=square, 5=filled square, 6=circle,
#            7=filled circle, 8=triangle, 9=filled triangle, etc.



# Parse Input Variables
my @fields;
my $line;
my %Data = ();

my $USE_NICE_COLORS_FLAG = 0;

# Commands for figure format
my $plot_name = "default";
my $xaxis_label  = "x-default"; #"Clusters";
my $yaxis_label  = "y-default"; #"Percentage (%)";
my $title        = "# default-title";
my $point_size   = "0.5";
my $line_size    = 1;
my $mutiply_data = 1; 
my $plot_with    = "points";
#my $title        = "";
my $use_titels   = "notitle"; #"title \"%s\" ";
my $mode         = "color enhanced"; # or color
my $log_usage    = "set yrange [0:]";

# -------------
my $tics         = "default";
my $range        = "default";

my $injected_lines = ""; # add any fancy line here

# Few colors that I know are nice
my @Colors = ( "n/a", 9, 1, 3, 7, -1, 0, 10, 11, 12, 13, 14, 15 );

open(IN_FILE, "$infile" ) || die("Could not open file: $!");
while ($line=<IN_FILE>)
{
    if ( $line =~ m/#/) 
    {
        next;
    }
    elsif ( $line =~ m/^\n/ )
    {
        next;
    }
    elsif ( $line =~ m/Opt=/i )
    {
        $_ = $';
        if ( m/name\s+(.*)/ )
        {
            $plot_name = $1;
        }
        elsif ( m/x-label\s+(.*)/ )
        {
            $xaxis_label = $1;
        }
        elsif ( m/use-nice-colors/)
        {
            $USE_NICE_COLORS_FLAG=1;
        }
        elsif ( m/mode\s+(.*)/ )        # Color or Black & White 
        {
            $mode = $1;
        }
        elsif ( m/y-label\s+(.*)/ )
        {
            $yaxis_label = $1;
        }
        elsif ( m/timeseries\s+(.*)/ )
        {
            $xaxis_label = "Time";
            $line_size = 2; 
            $plot_with = "linespoints";
            $point_size = 0.2;
            $title = "Time series presentation defalut parameters";
            $injected_lines = "set xdata time\n";
            $injected_lines .= "set timefmt \"%s\"\n";
            $injected_lines .= "set format x \" %d\\n%H:%M\"\n";
        }
        
        elsif ( m/add\s+(.*)/  )
        {
            $injected_lines .= "$1\n";
        }
        elsif ( m/psize\s+(.*)/ )       # Point size 
        {
            $point_size = $1;
        }
        elsif ( m/lsize\s+(.*)/ )    	# Line width
        {
            $line_size = $1;
        }
        elsif ( m/type\s+(.*)/ )      	# Plot with 
        {
            $plot_with = $1;
        }
        elsif ( m/title\s+(.*)/ )        # Plot with
        {
            $title = $1;
        }
        elsif ( m/tics\s+(.*)/ )
        {
            $tics = $1; 
        }
        elsif ( m/range\s+(.*)/ )
        {
            $range = $1;
        }
        elsif ( m/use-legent(.*)/ )
        {
            $use_titels  = "title \"%s\" ";
        }
        elsif ( m/use-log/ )
        {
            $log_usage = "set log x\nset log y\n";
        }

    }
    else
    {
        (@fields) = split(/\s+/, $line);
        push @{$Data{$fields[0]}} , [ @fields[1..$#fields] ];
    }

}
close(IN_FILE);
       


#TODO: test violation e.g. using lines with point sizes 


my $plot_out_name = "$plot_name.eps";
my $date = `date  +%Y-%m-%d-%H_%M_%S`; chomp($date);
#my @list_nice_lins ?
#my @luist_nice_points ?


open (OUT_FILE, ">$plot_name.gnu")  || die("Could not open file!");
my $gnu = `which gnuplot`;
chomp $gnu;
print OUT_FILE  "#!$gnu\n";
print OUT_FILE  "# $title $date\n";
print OUT_FILE  "set terminal postscript eps $mode \"Helvetica\" 22\n";
print OUT_FILE  "set output \"$plot_out_name\" \n";
print OUT_FILE  "set autoscale \n";
if ( $title ne "# default-title" ) {
    print OUT_FILE  "set title \"$title\" \n";
}
print OUT_FILE  "$log_usage\n";
print OUT_FILE  "#set key 60,6 \n";
print OUT_FILE  "set xlabel \"$xaxis_label\" \n";
print OUT_FILE  "set ylabel \"$yaxis_label\" \n";
if ( $tics !~ m/default/ )
{
    print OUT_FILE  "set xtics $tics \n";
}
if ( $range !~ m/default/ )
{
    print OUT_FILE  "set xrange $range  \n";
}
print OUT_FILE  "#set grid \n";
print OUT_FILE  "$injected_lines";
print OUT_FILE  "# set label 1 \"Y=AX^2+BX+C\" at 11,1225\n";
print OUT_FILE  "plot ";


my $first_flag = 1;
my $last = "";
my $counter = 4 ;

my @Data_Sequence_Order = ( sort keys %Data );

foreach my $app ( @Data_Sequence_Order ) 
{

    my $added_color_line = "";
    if ( $USE_NICE_COLORS_FLAG > 0 )
    {
        $added_color_line = "lt $Colors[$USE_NICE_COLORS_FLAG]";
        $USE_NICE_COLORS_FLAG++;
    }

    if ( $first_flag == 1) 
    {

        my $line_command = "";
        if ( $line_size eq "" ) { 
            
        } else {
            $line_command = "lw $line_size"
        }
        printf OUT_FILE "\"-\" using 1:2 $use_titels  with $plot_with $added_color_line pt $counter ps $point_size $line_command", $app;
        $first_flag = 0;
    }
    else 
    {
        printf OUT_FILE ", \"-\" using 1:2 $use_titels with $plot_with $added_color_line pt $counter ps $point_size lw $line_size", $app;
    }
    $last = $app;
    $counter++;
}
print OUT_FILE "\n";

# Write data values to gnuplot file 
foreach my $app ( @Data_Sequence_Order )
{
    foreach (sort { $b->[0] <=> $a->[0] } @{$Data{$app}})
    {
        my @line = @{$_};

        print OUT_FILE "@line\n";
    }
    print OUT_FILE "e\n" unless $app eq $last; 

}
print OUT_FILE "end\n";
print OUT_FILE "!epstopdf $plot_out_name\n";
close(OUT_FILE);

`gnuplot $plot_name.gnu; chmod 700 $plot_name.gnu`;

exit 0;

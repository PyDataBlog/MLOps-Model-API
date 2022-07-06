#!/usr/bin/perl

use strict;
use warnings;
use Image::Magick; 

my $DEBUG = 0;

my $err = '';

if (scalar(@ARGV) != 5){
	print "    usage:\n    $0 inputfile outputfile stepcount direction width_or_height\n";
	exit;
}

# move to command line args:
my $image_src_file			= $ARGV[0]; # button source filename
my $image_dst_file			= $ARGV[1];	# destination filename. Folders are required to exist
my $step_count 					= $ARGV[2]; # granularity of the fader movement
my $direction						= $ARGV[3]; # horisontal or vertical
my $user_size						= $ARGV[4];	# desired width (for horizontal) or height (for vertical)

# read button image
my $button = Image::Magick->new;
$err = $button->Read($image_src_file);
die "$err" if "$err";

my $command = 'convert -size ';
my $options = '';

my $dst_width;
my $dst_height;

if ($direction eq 'horizontal'){
	# determine dst height
	$dst_height = $step_count * $button->Get('height');

	# determine last position of button:
	my $last_position = $user_size - $button->Get('width');

	#determine the interval between each step
	my $step_interval = $last_position / $step_count;

	# prepare options
	my $x = 0; 
	my $y = 0;

	for(my $i = 0; $i<$step_count; $i++)
	{
		$options .= $image_src_file .' -geometry +'.$x.'+'.$y.' -composite -compose src-over ';
		$x += $step_interval; 
		$y += $button->Get('height');
	}
	$dst_width = $user_size;
}
elsif($direction eq 'vertical'){
	# determine dst height
	$dst_height = $step_count * $user_size;

	#determine the interval between each step
	my $step_interval = $user_size - (($user_size - $button->Get('height')) / $step_count);
	print "step_interval: $step_interval\n" if $DEBUG;

	# prepare options
	my $x = 0; 
	my $y = $user_size - $button->Get('height');

	for(my $i = 0; $i < $step_count; $i++)
	{
		print "y: $y\n" if $DEBUG;
		$options .= $image_src_file .' -geometry +'.$x.'+'.$y.' -composite -compose src-over ';
		$y += $step_interval;
	}
	$dst_width = $button->Get('width');
}
else
{
	die('Illegal direction specified');
}

$command .= $dst_width.'x'.$dst_height. ' xc:transparent '.$options.' '.$image_dst_file;

print 'command: '.$command."\n" if $DEBUG;

# ok, let's roll
my $result = `$command`;
print $result ? $result."\n" : "done\n";

exit;
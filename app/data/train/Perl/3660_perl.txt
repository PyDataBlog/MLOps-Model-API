#!/usr/bin/perl -w

=head

This is a little bit more of perl/tk.

=cut

use strict;
use warnings;

use Tk;

sub buttonhandler {
	print "Button clicked!\n";
}

my $main = MainWindow->new();
my $button = $main->Button(-text => "Click me to exit", -command => sub {$main->destroy()});
$button->pack();
my $button2 = $main->Button(-text => "Click me to print");
$button2->configure("-command" => \&buttonhandler );
$button2->pack();
Tk::MainLoop();
print "after main loop\n";

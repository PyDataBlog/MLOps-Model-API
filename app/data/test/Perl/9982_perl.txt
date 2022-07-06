#!/usr/bin/env perl

use strict;
use warnings;
use v5.012;


my $to_fix = qr/^(\$driver->find_element.*?"(?<elem>.+?)"\), "(?<type>.+?)"\)->(?<action>.*?);.*)/;
my $to_pause = qr/^(# ERROR:.*?Unsupported command \[getAllFields .*)/; 
my $to_action_chain = qr/^(# ERROR:.*?Unsupported command \[mouseMove \| (?<query>.+?) \| .*)/;

while (<STDIN>) {
  if ($_ =~ $to_fix) {
    print "benchit(\"$+{elem}, $+{type}\", sub { $1 });\n";

  } elsif ($_ =~ $to_pause) {
    print "\$driver->pause(1000);\n";

  } elsif ($_ =~ $to_action_chain) {
    my ($hover_type, @rest) = split /=/, $+{query};
    my $hover_query = join '=', @rest;
    $hover_query =~ s/@/\\@/;
    my $to_fix_line = <STDIN>;

    if (!defined $to_fix_line ||
        $to_fix_line !~ $to_fix) {
      next;
    }

    print <<"END";
{
  my \$hover_chain = Selenium::ActionChains->new(driver => \$driver);
  \$hover_chain->move_to_element(\$driver->find_element(Encode::decode('UTF-8', \"$hover_query\"), \"$hover_type\"));
  \$hover_chain->$+{action}(\$driver->find_element(Encode::decode('UTF-8', \"$+{elem}\"), \"$+{type}\"));
  benchit(\"$+{elem}, $+{type}\", sub { \$hover_chain->perform });
}
END
  } else {
    print $_;
  }
}

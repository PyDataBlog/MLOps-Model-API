use v5.08;
use strict;
use warnings;

use Contacts;
use Test::More tests => 3;
use List::Util qw/first/;

my $home_book = Contacts::init();
my $work_book = Contacts::init();

Contacts::add_contact( $home_book, 'Tom', { lives_in => 'USA', email => 'tomthecat@gmail.com' });
Contacts::add_contact( $home_book, 'Bob', { lives_in => 'USA', email => 'bob@gmail.com' });
	
Contacts::add_contact( $work_book, 'Mike', { lives_in => 'Mars', email => 'mike@gmail.com' });



my @result = Contacts::contacts_by_country( $home_book, 'USA' );




ok( first { $_ eq 'Tom' } @result, "Tom is in \@result");
ok( first { $_ eq 'Bob' } @result, "Bob is in \@result");
is( @result, 2, "Result has 2 items");

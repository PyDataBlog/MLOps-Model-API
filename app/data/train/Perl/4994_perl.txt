package Acme::Xyzzy;

# ABSTRACT: Nothing happens.

use strict;
use warnings;

use Exporter;
use vars qw(@ISA @EXPORT);

our $VERSION = "0.001";

@ISA = ('Exporter');
@EXPORT = ('xyzzy');


sub xyzzy {
	print "Nothing happens.\n";
}

1;

=head1 SYNOPSIS

	use Acme::Xyzzy;

	xyzzy;

=head1 DESCRIPTION

Nothing happens.

=head2 Methods

=over 12

=item C<< xyzzy() >>

Nothing happens.

=back

=head1 SEE ALSO

L<https://en.wikipedia.org/wiki/Xyzzy_%28computing%29>

=cut

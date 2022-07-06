# Copyright (c) 2005 Jens Luedicke <jensl@cpan.org>. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.

package Acme::True;

our $VERSION = '0.1';

use strict;
use warnings;

use constant TRUE => 1;
use constant FALSE => 0;
use constant REVERSE => 2;

sub new {
	my ($class) = @_;	
	my $self = bless [], $class;

	$self->[TRUE] = TRUE;
	$self->[FALSE] = FALSE;

	$self->[REVERSE] = FALSE;
	
	return $self;
}

sub set_reverse {
	my ($self,$rev) = @_;	
	$self->[REVERSE] = $rev;
}

sub get_reverse {
	my ($self) = @_;	
	return $self->[REVERSE];
}

sub true {
	my ($self) = @_;	

	if ($self->[REVERSE]) {
		return $self->[FALSE];
	}
	
	return $self->[TRUE];
}

sub false {
	my ($self) = @_;	

	if ($self->[REVERSE]) {
		return $self->[TRUE];
	}
	
	return $self->[FALSE];
}

sub untrue {
	my ($self) = @_;	

	if ($self->[REVERSE]) {
		return $self->[TRUE];
	}
	
	return $self->[FALSE];
}

sub unfalse {
	my ($self) = @_;	

	if ($self->[REVERSE]) {
		return $self->[FALSE];
	}
	
	return $self->[TRUE];
}

1;

=head1 NAME

Acme::True - (re)define the truth ;)


=head1 SYNOPSIS

Object-oriented true/false methods:

	use Acme::True;
	
	$true = new Acme::True;
	
	if ($true->true) {
		print "True!\n";
	}

	if ($true->false) {
		print "False!\n";
	}
	
	$true->set_reverse($true->true); 
	
	if ($true->true) {
		print "False!\n";
	}

	if ($true->false) {
		print "True!\n";
	}

=head1 METHODS

=over 4

=item C<new()>

Create a new Acme::True object

=item C<true()>

Returns true or false. true by default. 

=item C<false()>

Returns false or true. false by default. 

=item C<untrue()>

The opposite of C<true()>

=item C<unfalse()>

The opposite of C<false()>

=item C<set_reverse($boolean)>

Make that C<true> will return false and C<false> true!

=item C<get_reverse()>

Get the current boolean re-definition status.

=back

=head1 BUGS

Please mail the author if you encounter any bugs.

=head1 AUTHOR

Jens Luedicke E<lt>jensl@cpan.orgE<gt> web: L<http://perldude.de/>

=head1 CHANGES

Version 0.1: first release

=head1 COPYRIGHT AND LICENCE

Copyright (c) 2005 Jens Luedicke. All rights reserved. This program is free
software; you can redistribute it and/or modify it under the same terms as Perl
itself.

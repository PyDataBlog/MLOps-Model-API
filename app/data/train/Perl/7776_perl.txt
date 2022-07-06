package SSR::Model::Signature;

use strict;

use base qw(SSR::Model::Base);

__PACKAGE__->meta->setup(
    table   => 'signatures',

    columns => [
        id          => { type => 'integer', not_null => 1 },
        vorname     => { type => 'varchar' },
        nachname    => { type => 'varchar' },
        institution => { type => 'varchar' },
        titel       => { type => 'varchar' },
        ort         => { type => 'varchar' },
        email       => { type => 'varchar' },
        public      => { type => 'integer' },
        intouch     => { type => 'integer' },
        comment     => { type => 'varchar' },
        nonce       => { type => 'varchar' },
        active      => { type => 'integer' },
        upvoted		=> { type => 'integer' },
    ],

    primary_key_columns => [ 'id' ],
);

sub to_string {

	my $self = shift;
	my $s;


	if ( $self->vorname ) {

		$s = join ' ', grep { $_ } map { $self->$_ } qw/titel vorname nachname/;
		$s .= ', ' . $self->institution if $self->institution;

	}

	else {

		$s = $self->institution;

	}

	$s .= ', ' . $self->ort if $self->ort;

	return $s;

}

1;


package Tapper::Schema::TestrunDB::ResultSet::Precondition;
BEGIN {
  $Tapper::Schema::TestrunDB::ResultSet::Precondition::AUTHORITY = 'cpan:TAPPER';
}
{
  $Tapper::Schema::TestrunDB::ResultSet::Precondition::VERSION = '4.1.3';
}

use strict;
use warnings;

use parent 'DBIx::Class::ResultSet';
use YAML::Syck;


sub add {
        my ($self, $preconditions) = @_;

        my @precond_list = @$preconditions;
        my @precond_ids;

        foreach my $precond_data (@precond_list) {
                # (XXX) decide how to handle empty preconditions
                next if not (ref($precond_data) eq 'HASH');
                my $shortname    = $precond_data->{shortname} || '';
                my $timeout      = $precond_data->{timeout};
                my $precondition = $self->result_source->schema->resultset('Precondition')->new
                    ({
                      shortname    => $shortname,
                      precondition => Dump($precond_data),
                      timeout      => $timeout,
                     });
                $precondition->insert;
                push @precond_ids, $precondition->id;
        }
        return @precond_ids;
}

1;

__END__

=pod

=encoding utf-8

=head1 NAME

Tapper::Schema::TestrunDB::ResultSet::Precondition

=head2 add

Create (add) a list of preconditions and return them with their now
associated db data (eg. ID).

=head1 AUTHOR

AMD OSRC Tapper Team <tapper@amd64.org>

=head1 COPYRIGHT AND LICENSE

This software is Copyright (c) 2013 by Advanced Micro Devices, Inc..

This is free software, licensed under:

  The (two-clause) FreeBSD License

=cut

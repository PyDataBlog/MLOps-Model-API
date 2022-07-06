use utf8;
package Testbed::Spatial::VDS::Schema::Result::Public::DetectorCountsHr;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

Testbed::Spatial::VDS::Schema::Result::Public::DetectorCountsHr

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<public.detector_counts_hr>

=cut

__PACKAGE__->table("public.detector_counts_hr");

=head1 ACCESSORS

=head2 vds_id

  data_type: 'integer'
  is_nullable: 0

=head2 obs_day

  data_type: 'date'
  is_nullable: 0

=head2 truckcnt

  data_type: 'real'
  is_nullable: 1

=head2 vehcnt

  data_type: 'real'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "vds_id",
  { data_type => "integer", is_nullable => 0 },
  "obs_day",
  { data_type => "date", is_nullable => 0 },
  "truckcnt",
  { data_type => "real", is_nullable => 1 },
  "vehcnt",
  { data_type => "real", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</vds_id>

=item * L</obs_day>

=back

=cut

__PACKAGE__->set_primary_key("vds_id", "obs_day");


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-06 13:01:07
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:jHi1O3vsrxSDBdk9xUDVpA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;

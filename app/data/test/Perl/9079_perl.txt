use utf8;
package Testbed::Spatial::VDS::Schema::Result::Public::GeomPoints4326;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

Testbed::Spatial::VDS::Schema::Result::Public::GeomPoints4326

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<public.geom_points_4326>

=cut

__PACKAGE__->table("public.geom_points_4326");

=head1 ACCESSORS

=head2 gid

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 0

=head2 geom

  data_type: 'geometry'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "gid",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 0 },
  "geom",
  { data_type => "geometry", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</gid>

=back

=cut

__PACKAGE__->set_primary_key("gid");

=head1 RELATIONS

=head2 g

Type: belongs_to

Related object: L<Testbed::Spatial::VDS::Schema::Result::Public::GeomId>

=cut

__PACKAGE__->belongs_to(
  "g",
  "Testbed::Spatial::VDS::Schema::Result::Public::GeomId",
  { gid => "gid" },
  { is_deferrable => 0, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 vds_points_4326s

Type: has_many

Related object: L<Testbed::Spatial::VDS::Schema::Result::Public::VdsPoints4326>

=cut

__PACKAGE__->has_many(
  "vds_points_4326s",
  "Testbed::Spatial::VDS::Schema::Result::Public::VdsPoints4326",
  { "foreign.gid" => "self.gid" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 wim_points_4326s

Type: has_many

Related object: L<Testbed::Spatial::VDS::Schema::Result::Public::WimPoints4326>

=cut

__PACKAGE__->has_many(
  "wim_points_4326s",
  "Testbed::Spatial::VDS::Schema::Result::Public::WimPoints4326",
  { "foreign.gid" => "self.gid" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-06 13:01:07
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:3dW7wpCkwzhLagcfxbZQJw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;

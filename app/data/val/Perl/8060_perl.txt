use utf8;
package Testbed::Spatial::VDS::Schema::Result::Public::VdsPoints4269;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

Testbed::Spatial::VDS::Schema::Result::Public::VdsPoints4269

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<public.vds_points_4269>

=cut

__PACKAGE__->table("public.vds_points_4269");

=head1 ACCESSORS

=head2 gid

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 0

=head2 vds_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "gid",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 0 },
  "vds_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 0 },
);

=head1 PRIMARY KEY

=over 4

=item * L</vds_id>

=back

=cut

__PACKAGE__->set_primary_key("vds_id");

=head1 RELATIONS

=head2 g

Type: belongs_to

Related object: L<Testbed::Spatial::VDS::Schema::Result::Public::GeomPoints4269>

=cut

__PACKAGE__->belongs_to(
  "g",
  "Testbed::Spatial::VDS::Schema::Result::Public::GeomPoints4269",
  { gid => "gid" },
  { is_deferrable => 0, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 vd

Type: belongs_to

Related object: L<Testbed::Spatial::VDS::Schema::Result::Public::VdsIdAll>

=cut

__PACKAGE__->belongs_to(
  "vd",
  "Testbed::Spatial::VDS::Schema::Result::Public::VdsIdAll",
  { id => "vds_id" },
  { is_deferrable => 0, on_delete => "NO ACTION", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-06 13:01:07
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:eugw0QaXR4EodixDsVH2AA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;

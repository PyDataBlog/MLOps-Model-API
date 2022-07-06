use utf8;
package CollectiveAccess::PortaleMusica::Schema::Result::CaTourStopsXOccurrence;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

CollectiveAccess::PortaleMusica::Schema::Result::CaTourStopsXOccurrence

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 COMPONENTS LOADED

=over 4

=item * L<DBIx::Class::InflateColumn::DateTime>

=back

=cut

__PACKAGE__->load_components("InflateColumn::DateTime");

=head1 TABLE: C<ca_tour_stops_x_occurrences>

=cut

__PACKAGE__->table("ca_tour_stops_x_occurrences");

=head1 ACCESSORS

=head2 relation_id

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 occurrence_id

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 stop_id

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 type_id

  data_type: 'smallint'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 source_info

  accessor: 'column_source_info'
  data_type: 'longtext'
  is_nullable: 0

=head2 sdatetime

  data_type: 'decimal'
  is_nullable: 1
  size: [30,20]

=head2 edatetime

  data_type: 'decimal'
  is_nullable: 1
  size: [30,20]

=head2 label_left_id

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 1

=head2 label_right_id

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 1

=head2 rank

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "relation_id",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "occurrence_id",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "stop_id",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "type_id",
  {
    data_type => "smallint",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "source_info",
  {
    accessor    => "column_source_info",
    data_type   => "longtext",
    is_nullable => 0,
  },
  "sdatetime",
  { data_type => "decimal", is_nullable => 1, size => [30, 20] },
  "edatetime",
  { data_type => "decimal", is_nullable => 1, size => [30, 20] },
  "label_left_id",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 1,
  },
  "label_right_id",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 1,
  },
  "rank",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
);

=head1 PRIMARY KEY

=over 4

=item * L</relation_id>

=back

=cut

__PACKAGE__->set_primary_key("relation_id");

=head1 UNIQUE CONSTRAINTS

=head2 C<u_all>

=over 4

=item * L</occurrence_id>

=item * L</stop_id>

=item * L</type_id>

=item * L</sdatetime>

=item * L</edatetime>

=back

=cut

__PACKAGE__->add_unique_constraint(
  "u_all",
  ["occurrence_id", "stop_id", "type_id", "sdatetime", "edatetime"],
);

=head1 RELATIONS

=head2 label_left

Type: belongs_to

Related object: L<CollectiveAccess::PortaleMusica::Schema::Result::CaTourStopLabel>

=cut

__PACKAGE__->belongs_to(
  "label_left",
  "CollectiveAccess::PortaleMusica::Schema::Result::CaTourStopLabel",
  { label_id => "label_left_id" },
  {
    is_deferrable => 1,
    join_type     => "LEFT",
    on_delete     => "RESTRICT",
    on_update     => "RESTRICT",
  },
);

=head2 label_right

Type: belongs_to

Related object: L<CollectiveAccess::PortaleMusica::Schema::Result::CaOccurrenceLabel>

=cut

__PACKAGE__->belongs_to(
  "label_right",
  "CollectiveAccess::PortaleMusica::Schema::Result::CaOccurrenceLabel",
  { label_id => "label_right_id" },
  {
    is_deferrable => 1,
    join_type     => "LEFT",
    on_delete     => "RESTRICT",
    on_update     => "RESTRICT",
  },
);

=head2 occurrence

Type: belongs_to

Related object: L<CollectiveAccess::PortaleMusica::Schema::Result::CaOccurrence>

=cut

__PACKAGE__->belongs_to(
  "occurrence",
  "CollectiveAccess::PortaleMusica::Schema::Result::CaOccurrence",
  { occurrence_id => "occurrence_id" },
  { is_deferrable => 1, on_delete => "RESTRICT", on_update => "RESTRICT" },
);

=head2 stop

Type: belongs_to

Related object: L<CollectiveAccess::PortaleMusica::Schema::Result::CaTourStop>

=cut

__PACKAGE__->belongs_to(
  "stop",
  "CollectiveAccess::PortaleMusica::Schema::Result::CaTourStop",
  { stop_id => "stop_id" },
  { is_deferrable => 1, on_delete => "RESTRICT", on_update => "RESTRICT" },
);

=head2 type

Type: belongs_to

Related object: L<CollectiveAccess::PortaleMusica::Schema::Result::CaRelationshipType>

=cut

__PACKAGE__->belongs_to(
  "type",
  "CollectiveAccess::PortaleMusica::Schema::Result::CaRelationshipType",
  { type_id => "type_id" },
  { is_deferrable => 1, on_delete => "RESTRICT", on_update => "RESTRICT" },
);


# Created by DBIx::Class::Schema::Loader v0.07043 @ 2015-09-01 16:35:15
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Z7o+38D/vheG4NXXXeBMqg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;

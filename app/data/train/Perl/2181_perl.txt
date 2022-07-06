use utf8;
package CollectiveAccess::PortaleMusica::Schema::Result::CaDataImportEvent;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

CollectiveAccess::PortaleMusica::Schema::Result::CaDataImportEvent

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

=head1 TABLE: C<ca_data_import_events>

=cut

__PACKAGE__->table("ca_data_import_events");

=head1 ACCESSORS

=head2 event_id

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 occurred_on

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 user_id

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 1

=head2 description

  data_type: 'text'
  is_nullable: 0

=head2 type_code

  data_type: 'char'
  is_nullable: 0
  size: 10

=head2 source

  data_type: 'text'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "event_id",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "occurred_on",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 0 },
  "user_id",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 1,
  },
  "description",
  { data_type => "text", is_nullable => 0 },
  "type_code",
  { data_type => "char", is_nullable => 0, size => 10 },
  "source",
  { data_type => "text", is_nullable => 0 },
);

=head1 PRIMARY KEY

=over 4

=item * L</event_id>

=back

=cut

__PACKAGE__->set_primary_key("event_id");

=head1 RELATIONS

=head2 ca_data_import_event_logs

Type: has_many

Related object: L<CollectiveAccess::PortaleMusica::Schema::Result::CaDataImportEventLog>

=cut

__PACKAGE__->has_many(
  "ca_data_import_event_logs",
  "CollectiveAccess::PortaleMusica::Schema::Result::CaDataImportEventLog",
  { "foreign.event_id" => "self.event_id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 ca_data_import_items

Type: has_many

Related object: L<CollectiveAccess::PortaleMusica::Schema::Result::CaDataImportItem>

=cut

__PACKAGE__->has_many(
  "ca_data_import_items",
  "CollectiveAccess::PortaleMusica::Schema::Result::CaDataImportItem",
  { "foreign.event_id" => "self.event_id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 user

Type: belongs_to

Related object: L<CollectiveAccess::PortaleMusica::Schema::Result::CaUser>

=cut

__PACKAGE__->belongs_to(
  "user",
  "CollectiveAccess::PortaleMusica::Schema::Result::CaUser",
  { user_id => "user_id" },
  {
    is_deferrable => 1,
    join_type     => "LEFT",
    on_delete     => "RESTRICT",
    on_update     => "RESTRICT",
  },
);


# Created by DBIx::Class::Schema::Loader v0.07043 @ 2015-09-01 16:35:14
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:nW5sQtYDGeAmTfbCzIuFKA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;

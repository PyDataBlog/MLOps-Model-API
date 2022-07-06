use utf8;
package CollectiveAccess::PortaleMusica::Schema::Result::CaBookmarkFolder;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

CollectiveAccess::PortaleMusica::Schema::Result::CaBookmarkFolder

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

=head1 TABLE: C<ca_bookmark_folders>

=cut

__PACKAGE__->table("ca_bookmark_folders");

=head1 ACCESSORS

=head2 folder_id

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 name

  data_type: 'varchar'
  is_nullable: 0
  size: 255

=head2 user_id

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 rank

  data_type: 'smallint'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "folder_id",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "name",
  { data_type => "varchar", is_nullable => 0, size => 255 },
  "user_id",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 0 },
  "rank",
  {
    data_type => "smallint",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
);

=head1 PRIMARY KEY

=over 4

=item * L</folder_id>

=back

=cut

__PACKAGE__->set_primary_key("folder_id");


# Created by DBIx::Class::Schema::Loader v0.07043 @ 2015-09-01 16:35:14
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:5iQLiyn0jJ2w3A+htR8HhA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;

package Lorem::Meta::Attribute::Trait::Inherit;
{
  $Lorem::Meta::Attribute::Trait::Inherit::VERSION = '0.23.1';
}
use Moose::Role;

package Moose::Meta::Attribute::Custom::Trait::Inherit;
{
  $Moose::Meta::Attribute::Custom::Trait::Inherit::VERSION = '0.23.1';
}
sub register_implementation {'Lorem::Meta::Attribute::Trait::Inherit'}

1;

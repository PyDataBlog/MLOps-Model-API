package Tapper::PRC;
# git description: v4.1.1-11-g75f1086

BEGIN {
  $Tapper::PRC::AUTHORITY = 'cpan:TAPPER';
}
{
  $Tapper::PRC::VERSION = '4.1.2';
}
# ABSTRACT: Tapper - Program run control for test program automation

use strict;
use warnings;

use IO::Socket::INET;
use YAML::Syck;
use Moose;
use Log::Log4perl;
use URI::Escape;

extends 'Tapper::Base';
with 'MooseX::Log::Log4perl';

has cfg => (is      => 'rw',
            isa     => 'HashRef',
            default => sub { {} },
           );
with 'Tapper::Remote::Net';


sub mcp_error
{

        my ($self, $error) = @_;
        $self->log->error($error);
        my $retval = $self->mcp_inform({status => 'error-testprogram', error => $error});
        $self->log->error($retval) if $retval;
        exit 1;
};

1;

__END__
=pod

=encoding utf-8

=head1 NAME

Tapper::PRC - Tapper - Program run control for test program automation

=head1 DESCRIPTION

This distribution implements a program run control for test program
automation. It is part of the Tapper distribution.

=head1 FUNCTIONS

=head2 mcp_error

Log an error and exit.

@param string - messages to send to MCP

@return never returns

=head1 AUTHOR

AMD OSRC Tapper Team <tapper@amd64.org>

=head1 COPYRIGHT AND LICENSE

This software is Copyright (c) 2012 by Advanced Micro Devices, Inc..

This is free software, licensed under:

  The (two-clause) FreeBSD License

=cut


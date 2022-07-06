package Net::Amazon::Signature::V3;

# ABSTRACT: Sign AWS requests -- V3

use utf8;

use Moose;
use namespace::autoclean;
use MooseX::AttributeShortcuts 0.015;
use MooseX::Types::Common::String ':all';

use HTTP::Date;
use Digest::HMAC_SHA1;
use MIME::Base64;

has $_ => (is => 'ro', isa => NonEmptySimpleStr, required => 1)
    for qw{ id key };

has trust_local_time => (is => 'ro', isa => 'Bool', builder => sub { 0 });

# see route53-dg-20120229.pdf, page 98
sub _time_string { time2str time }

=method signed_headers

Returns a list of several key-value pairs suitable for including directly as
headers.  These headers will authenticate the request to Amazon.

Note that these headers are only good when used within 5 minutes of the time
that Amazon thinks it is.

This routine is largely based off code extracted from
L<Net::Amazon::Route53/request>.

=cut

sub signed_headers {
    my $self = shift @_;

    my $hmac = Digest::HMAC_SHA1->new($self->key);
    my $date = $self->_time_string;

    $hmac->add($date);
    my $signature = encode_base64($hmac->digest, '');

    my $auth_string
        = 'AWS3-HTTPS '
        . 'AWSAccessKeyId=' . $self->id . ','
        . 'Algorithm=HmacSHA1,'
        . 'Signature=' . $signature
        ;

    my %options = (
        'x-amz-date'           => $date,
        'X-Amzn-Authorization' => $auth_string,
    );

    ### %options
    return %options;
}

__PACKAGE__->meta->make_immutable;
!!42;
__END__

=for stopwords AWS

=head1 SYNOPSIS

    # somewhere inside the depths of your code...
    my $signer = Net::Amazon::Signature::V3->new(id => $id, key => $key);
    my $req = HTTP::Request->new('GET', $uri, [ $signer->signed_headers ]);

    # profit!

=head1 DESCRIPTION

Amazon requires authentication when interfacing with its web services; this
package implements V3 of Amazon's authentication schemes.

=head1 SEE ALSO

Net::Amazon::Route53

=cut

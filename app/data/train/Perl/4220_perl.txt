package Error::Pure::Bio;

# Pragmas.
use base qw(Exporter);
use strict;
use warnings;

# Modules.
use Error::Pure::Utils qw(err_helper);
use Error::Pure::Output::Bio qw(err_bio);
use List::MoreUtils qw(none);
use Readonly;

# Constants.
Readonly::Array our @EXPORT_OK => qw(err);
Readonly::Scalar my $EVAL => 'eval {...}';

# Version.
our $VERSION = 0.01;

# Ignore die signal.
$SIG{__DIE__} = 'IGNORE';

# Process error.
sub err {
	my @msg = @_;

	# Get errors structure.
	my @errors = err_helper(@msg);

	# Finalize in main on last err.
	my $stack_ar = $errors[-1]->{'stack'};
	if ($stack_ar->[-1]->{'class'} eq 'main'
		&& none { $_ eq $EVAL || $_ =~ m/^eval '/ms }
		map { $_->{'sub'} } @{$stack_ar}) {

		die err_bio(@errors);

	# Die for eval.
	} else {
		my $e = $errors[-1]->{'msg'}->[0];
		chomp $e;
		die "$e\n";
	}
	return;
}

1;

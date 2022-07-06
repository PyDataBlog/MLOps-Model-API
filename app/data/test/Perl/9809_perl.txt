#!/usr/bin/perl
use strict;
use warnings;
use Carp;
use Getopt::Long;

my $script = "compress-wordpress";
my $scriptPath;
my $force = 0;

GetOptions('force'=>\$force);

if (-e "/usr/local/sbin/$script") {
    checkExisting("/usr/local/sbin/$script", $script);
    $scriptPath = "/usr/local/sbin/$script";
} elsif (-e "/usr/sbin/$script") {
    checkExisting("/usr/sbin/$script", $script);
    $scriptPath = "/usr/sbin/$script";
} else {
    if (install("/usr/local/sbin/$script", $script)) {
        print "Installed /usr/local/sbin/$script\n";
    }
    $scriptPath = "/usr/local/sbin/$script";
}

system("chmod +x $scriptPath");

1;

sub checkExisting {
    my $path = shift;
    my $script = shift;
    my $scriptPath;
    if (!$force && `$path --zysys` !~ /zysys/) {
        croak "Please confirm $path represents the correct file to replace.\nIf yes, rerun this script using --force";
        exit;
    }
    if (!check_md5($path, $script)) {
        install($path, $script);
        print "Upgraded $path\n";
        return 1;
    } else {
        print "Existing file $path is identical to $script\n";
        return 0;
    }
}

sub check_md5 {
    my @hashes = ();
    map { push @hashes, `cat $_ | tr --delete [:space:] | md5sum` } @_;
    my $i = 0;
    map { if ($hashes[$i+1]) { return 0 if $_ ne $hashes[$i+1] }; $i++;} @hashes;
    return 1;
}

sub install {
    my $scriptPath = shift;
    my $script = shift;
    system("cp $script $scriptPath");
    return 1;
}

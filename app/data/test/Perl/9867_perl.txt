######
my $script_version="0.1.0";

use Getopt::Std;
use strict;
use warnings;

## either fetch CRAN or Bioconductor, defaults to pull.
## push pushes a local mirror to another directory.

my $cran="cran.r-project.org::CRAN";       ## that's the url to the CRAN
my $bioc="master.bioconductor.org::3.0";  ## that's the url from which we fetch bioconductor.
my $localcran="/Volumes/jodata/mirror/CRAN";     ## that's where we store locally the CRAN.
my $localbioc="/Volumes/jodata/mirror/bioconductor/bioc_3.0";     ## that's where we store locally the bioconductor.

my $localurl;
my $remoteurl;

my %option=();
# remote repo, local repo.
getopts("l:r:s:h",\%option);

my $switch = "bioc";
## define which files/folders we would like to exclude.
##my $excludes = "--exclude '**windows*' --exclude '**macosx/' --exclude '*data/experiment*'";
my $excludes = "--exclude '**windows*' --exclude '**macosx/' --exclude '**linux*' --exclude 'bin/*' --exclude 'bioc/bin*' --exclude 'bioc/citations*' --exclude 'bioc/html*' --exclude 'bioc/install*' --exclude 'bioc/licenses*' --exclude 'bioc/manuals*' --exclude 'bioc/news*' --exclude 'bioc/readmes*' --exclude 'bioc/vignettes*' --exclude 'contrib/Archive*'";


if( $option{ h } ){
## print help... obviously...
  print( "\nmirrror.pl version ".$script_version.".\n" );
  print( "Mirrors the CRAN or Bioconductor repository.\n");
  print( "usage: mirrror.pl [-hlrs]\n" );
  print( "Parameters:\n" );
  print( "h : print this help.\n" );
  print( "l : path to local copy of the repository. If not provided the default defined within this script will be used.\n" );
  print( "r : url of the remote repository. If not provided the default defined within this script will be used.\n" );
  print( "s : switch to define whether Bioconductor (the default) or CRAN should be mirrored. If defined has to be one of 'bioc' or 'CRAN'.\n\n" );
  exit 0;
}

if( defined( $option{ s } ) ){
  if( $option{ s } eq "CRAN" ){
    $switch = "CRAN";
  }
  elsif( $option{ s } eq "bioc" ){
    $switch = "bioc";
  }
  else{
    die( "parameter s has to be either 'bioc' or 'CRAN'!" );
  }
}

## define remote and local url based on switch.
if( $switch eq "CRAN" ){
  $localurl = $localcran;
  $remoteurl = $cran;
}
if( $switch eq "bioc" ){
  $localurl = $localbioc;
  $remoteurl = $bioc;
}

## eventually override the default
if( defined( $option{ r } ) ){
  $remoteurl = $option{ r };
}
if( defined( $option{ l } ) ){
  $localurl = $option{ l };
}

## download the biocLite.R and modify!
my @tmp = split /\//,$localurl;
my $basepath = join "/", @tmp[0..(scalar(@tmp)-2)];
if( system( "cd $basepath && rm -f biocLite.R && wget http://bioconductor.org/biocLite.R" )!=0 ){
  die "Error: $!\n";
}
## add lines that point to the local CRAN and bioconductor.
open( INSTALL, ">> $basepath/biocLite.R");
print INSTALL "#options(\"repos\"= c(CRAN=\"file://$basepath/CRAN\"))\n";
print INSTALL "#options(\"BioC_mirror\"=\"file://$basepath/bioconductor\")\n";
print INSTALL "options(\"repos\"= c(CRAN=\"file://data/shared/bioinf/mirror/CRAN/\"))\n";
print INSTALL "options(\"BioC_mirror\"=\"file://data/shared/bioinf/mirror/bioconductor\")\n";
close( INSTALL );


## ok, call the rsync.
print "Syncing $remoteurl to $localurl\n\n";

if( system( "rsync -rtlv --delete $excludes $remoteurl $localurl" )!=0 ){
  die "Error while running rsync: $!\n";
}


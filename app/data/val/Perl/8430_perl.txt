#!/usr/bin/perl
#use warnings;
use Data::Dumper;
use String::Util 'trim';
use File::DirWalk;
use LWP::UserAgent;
use URI::Escape;
use JSON;
my $ua = LWP::UserAgent->new;

sub camel_to_underscore {
    $_ = shift;
    # Replace first capital by lowercase
    # if followed my lowercase.
    s/^([A-Z])([a-z])/lc($1).$2/e;
    # Substitute camelCase to camel_case
    s/([a-z])([A-Z])/$1.'_'.lc($2)/ge;
    return $_;
}


sub generate_test_parameters {
    my $base = shift @_;
    my $plugin_path = shift @_;
    my $path = $base . $plugin_path;
    my $plugin_name = $plugin_path;
    $plugin_name =~ s/.pm//g;
    $plugin_name =~ s/\//::/g;
    open my $PLUGINPM, "<", $path or die $!;
    my $plugin_info = {'name' => $plugin_name};
    if ($plugin_name =~ /Spice/)
    {
	my @kickstart_arr = split('::',$plugin_name);
	my $kickstart = '/js/spice/'.$kickstart_arr[-1];
	$kickstart = camel_to_underscore($kickstart);
	$kickstart = lc $kickstart;
	$plugin_info->{'kickstart'} = $kickstart;
    }
    my @examples = ();
    foreach $line (<$PLUGINPM>)
    {
	if ($line =~/(primary_example_queries|secondary_example_queries)/)
	{
	    $line =~s/(primary_example_queries|secondary_example_queries)//g;
	    $line =~ s/;//g;
	    @eg = split(',', $line);
	    foreach $e (@eg)
	    {
		$e =~ s/(\"|\')//g;
		if (! ($e =~ ''))
		{
		push(@examples, trim($e));
		}
	    }
	}
	if ($line =~/zci/ && $line =~ /answer_type/)
	{
	    $line =~ s/(zci|answer_type|=>)//g;
	    $line =~s/(\"|\'|\;)//g;
	    $plugin_info->{'answer_type'} = trim($line);
	}
    }
    $plugin_info->{'examples'} = \@examples;
    return $plugin_info;

}

sub walk_repo {
    my $path = shift @_;
    $path = $path . '/lib/';
    my $dw = new File::DirWalk;
    @package_list = ();
    $dw->onDirEnter(sub {
	my ($dir) = @_;
	return File::DirWalk::SUCCESS;
		    });
    $dw->onFile(sub {
	my ($file) = @_;
	if ($file !~ /(Bundle|Role)/)
	{
	    my @module = split($path, $file);
	    push(@package_list,generate_test_parameters($path, $module[1]));
	}
	return File::DirWalk::SUCCESS;
		});

    $dw->walk($path);
    return \@package_list;
}

sub test_query {
    my $test_subject = shift @_;
    my $verbose = shift @_;
    my $plugin_name = $test_subject->{'name'};
    print "\n".$plugin_name."\n";
    my $examples_ref = $test_subject->{'examples'};
    my @examples = @{$examples_ref};
    if ($examples =~ 0)
	{
	    print "No Tests\n";
	    return;
	}
	
    if (exists $test_subject->{'answer_type'})
    {
	my $answer_type = $test_subject->{'answer_type'};
	#print @examples;
	foreach $ex (@examples)
	{
	    my $cooked_ex = uri_escape($ex);
	    my $url = 'https://api.duckduckgo.com?q='.$cooked_ex.'&o=json&no_html=1&d=0';
	    my $req = HTTP::Request->new(GET => $url);
	    my $res = $ua->request($req);
	    if ($res-> is_success){
		my $test_res =  from_json($res->content);
		if ($test_res->{"AnswerType"} =~ $answer_type)
		{
		    print '  '. $ex . " Test OK\n";
		}
		else
		{
		    print '  '. $ex . " Test FAIL\n";
		}
	       }
	    else
	    {
		print "  ". $ex . "Network Error\n";
	    }
	}
	
    }
    else
    {
	if ($plugin_name =~ /Goodie/)
	{
	    return;
	}
	if (exists $test_subject->{'kickstart'})
	{
	    my $kickstart = $test_subject->{'kickstart'};
	    foreach $ex (@examples)
	    {
		my $cooked_ex = uri_escape($ex);
		my $url = 'https://duckduckgo.com?q='.$cooked_ex;
		my $req = HTTP::Request->new(GET => $url);
		my $res = $ua->request($req);
		if ($res-> is_success){
		    
		    if ($res->content =~ /\Q$kickstart\E/)
		    {
			print '  '. $ex . " Test OK\n";
		    }
		    else
		    {
			print '  '. $ex . " Test FAIL\n";
		    }
		}
		else
		{
		    print "  ". $ex . "Network Error\n";
		}
	    }
	}
	    
    }
}   




my $repository = pop(@ARGV);
if ($repository =~ /...\/$/)
{
    my $last = chop($repository);
}

my $test1 = walk_repo($repository);
foreach $test (@{$test1})
{
    #print Dumper($test);
    print test_query($test, 0);
}



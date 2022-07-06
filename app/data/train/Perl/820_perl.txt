#!/usr/bin/perl
use CGI qw(:standard);
print header,
start_html({
		-title=>"my title",
		-alink=>"red",
		-vlink=>"black",
		-bgcolor=>"yellow" }),
h1('Hello World!'),
hr({-size=>"5",-width=>"70%",-color=>"red"}),
h2('Fall semester'),
ol(
	li('Perl'),
	li('PHP'),
	li('Python'),
),
hr,
h1('Course Roadmap'),
table({-border=>'1',-width=>"100%"},
	caption(strong('Use this table to plan your schedule')),
	Tr({-align=>CENTER,-valign=>TOP},
		[
			th(['\\','Fall','Spring','Summer']),
			th('2010').td(['no','yes','yes']),
			th('2011').td(['no','no','yes']),
			th('2012').td(['yes','yes','yes'])
		] )
), hr, a({
		-href=>"http://www.perl.org"},
	img({
			-src=>"../images/olympics.jpg",
			-width=>"300",
			-border=>"0",
			-alt=>"Olympics"})
), p,
a({
		-href=>"http://www.perl.org"},
	"Perl.org main web site"), p;
end_html;

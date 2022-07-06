#!/usr/bin/perl -w

$childs = shift @ARGV || 
	die "usage: ex8.pl <childs_count>";
$|=1;

for ($i=0; $i<$childs; $i++) {
	$pid = fork();
	if ($pid>0) {
		#parent
		push(@pids,$pid);
		next;
	} else {
		#child
		$t = rand 20;
		print "child $i is going to sleep for $t secs\n";
		srand;
		sleep($t);
		print "child $i is done\n";
		exit;
	}
}
print "pids: @pids\n";
#waitpid($pid,0) while ($pid = pop @pids);
while (wait()>-1) {}
print "parent is done\n";

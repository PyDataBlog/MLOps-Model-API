#!/usr/bin/perl -w
#Author Joe Bach
#Licence GPLv3
#Description If you want it "burned" this will take care of it for you.
#Note: does not deal with the disk so files might still be recoverable.

#Get file from command line

my $file = shift;
unless($file) {die "You need to feed the beast with a file";}
my $size = -s $file;
my $buffer = "";
my $queue = $size;



print "burning ".$file."\n";
open(TARGET,">",$file) or die "Cannot access this file, Does it exsist?\n";
close(TARGET);

sub burnit {
        open(TARGET,">",$file);
        print TARGET $buffer;
        close(TARGET);
        $buffer = "";
}

sub makeit {
        $queue = $size;
        while($queue) {
                $buffer=$buffer.pack("c",$_[0]);
        }
        &burnit;
        $queue--;
}

#Write random bits to file 7 times
for(my $i=0;$i<7;$i++) {
        &makeit(rand(127));
}
#write 1's to file
&makeit(1);
#write 0's to file
&makeit(0);
#delete file
unlink glob $file;
#make fake copy of file and remove it 7 times
for(my $i=0;$i<7;$i++) {
        &makeit(rand(127));
        unlink glob $file;
}

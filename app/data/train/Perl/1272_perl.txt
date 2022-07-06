#write a program that randomly chooses one of four nucleotides (ATCG)
#and then keeps prompting the user until you guess the correct one.
#doesn't need to be a subroutine.
#
print "S. Barclay\n";
print "BIFS617 Week 8 Conference 1\n\n";
#
#Randomly generate nucleotide
@nucleotide = ('A','T','C','G');
$base=$nucleotide[rand @nucleotide];
print "\nPick your poison: A, T, C, or G?\n";
$answer=<STDIN>;
chomp $answer;
while ($answer ne $base) {print "We're out of that particluar brew, matey. What else can I get you?\n";
                          $answer=<STDIN>; chomp $answer};
print "Arrgh! $answer it is!\n";
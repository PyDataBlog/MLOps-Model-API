# Exercise 14.2

# Multiply two 2x2 arrays together in a subroutine

$arr1 = [[1, 2], [3, 4]];
$arr2 = [[4, 3], [2, 1]];

&mult2x2($arr1, $arr2);

sub mult2x2 {
	my($x, $y) = @_;
	my $z;

	print("Array 1:\n");
	&print2x2($x);
	print("\nArray 2:\n");
	&print2x2($y);

	$z->[0][0] = $x->[0][0] * $y->[0][0] + $x->[0][1] * $y->[1][0];
	$z->[0][1] = $x->[0][0] * $y->[0][1] + $x->[0][1] * $y->[1][1];
	$z->[1][0] = $x->[1][0] * $y->[0][0] + $x->[1][1] * $y->[1][0];
	$z->[1][1] = $x->[1][0] * $y->[0][1] + $x->[1][1] * $y->[1][1];

	print("\nArray 1 times Array 2\n");
	&print2x2($z);
}

sub print2x2 {
	my($arr) = @_;
	my($i, $j);
	for $i (0,1) {
		for $j (0, 1) {
			printf("%2d ", $arr->[$i][$j]);
		}
		print("\n");
	}
}

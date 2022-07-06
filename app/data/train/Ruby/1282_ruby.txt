=begin
give highest possible product out of three integers in a given array
input: array of integers (will always have at least three integers)
output: highest product, integer

PSEUDOCODE
int_array.combination(3) => gives array of combinations of all possible groups of three
array_combos.each do |group|
  products_array << group.inject(:*)
end
products_array.sort
highest_product = products_array[-1]

O(n) run time, O(n^2) space
how to save space?

=end

def highest_product(array_of_ints)
  products_array = []
  array_of_ints.combination(3) {|group| products_array << group.inject(:*)}
  products_array.sort!
  highest_product = products_array.last
end

# O(n) run time, O(n) space

#DRIVER CODE

p highest_product([1,1,1,1,4]) == 4
p highest_product([7, 1, 9, 4]) == 252
p highest_product([-10, -10, 3, 1, 2]) == 300

# Concatenate Two Arrays

# I worked on this challenge by myself.


# Your Solution Below

def array_concat(array_1, array_2)
  new_array = []
  n = 0
  array_1.each do |x|
    new_array[n] = x
    n = n + 1
  end
  array_2.each do |x|
    new_array[n] = x
    n = n + 1
  end
  return new_array
end
  #method 2 (easiest, but no iteration)
  #new_array = array_1 + array_2
  ##method 3 (easier, uses push)
  ##new_array = []
  ##array_1.each do |x|
  ##  new_array << x
  ##end
  ##array_2.each do |y|
  ##  new_array << y
  ##end
  ##return new_array
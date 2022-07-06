# Longest String

# I worked on this challenge [by myself].

# longest_string is a method that takes an array of strings as its input
# and returns the longest string
#
# +list_of_words+ is an array of strings
# longest_string(list_of_words) should return the longest string in +list_of_words+
#
# If +list_of_words+ is empty the method should return nil


# Your Solution Below
def longest_string(array)
  x = ( array.length - 1 )
  
  if x < 0 
    return nil
  else 
    while x > 0
      if array[x].length < array[(x-1)].length
        array.delete_at(x)
        x = ( array.length - 1 )
      else
        array.delete_at((x-1))
        x = ( array.length - 1 )
      end
    end
  end
  
  return array[x]

end

#where I test it: 
array = ["helloss","this","is","a","hellose"]

puts longest_string(array)
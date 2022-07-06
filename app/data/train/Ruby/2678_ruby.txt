# Pad an Array

# I worked on this challenge [with: Alan Alcesto]

# I spent [1.5] hours on this challenge. (50 min peer pairing plus research before)


# Complete each step below according to the challenge directions and
# include it in this file. Also make sure everything that isn't code
# is commented in the file.



# 0. Pseudocode

# What is the input?
# The input would be three arguments: an array, the minimum size, and an optional value.
# What is the output? (i.e. What should the code return?)
# The output would be the array filled in with the optional value if the minimum size is more than the array length.
# What are the steps needed to solve the problem?
# pad!:
# - the method accepts 3 arguments (array, minimum size, optional value)
# - IF the minimum size is less than or equal to the length of the array it will return the original array
# - ELSE if the minimum size is larger than the array length, it will increase the array by the difference in the minimum size and the array length
# - with the empty spots in the array added from the minimum size, the empty spots will be replaced with the optional value
# - it will return the array with the added spots and optional value if needed, changes are permanent
# pad:
# - the method accepts 3 arguments (array, minimum size, optional value)
# - IF the minimum size is less than or equal to the length of the array it will return the original array
# - ELSE if the minimum size is larger than the array length, it will increase the array by the difference in the minimum size and the array length
# - with the empty spots in the array added from the minimum size, the empty spots will be replaced with the optional value
# - it will return the array with the added spots and optional value if needed, changes are not permanent

# 1. Initial Solution

=begin
def pad!(array, min_size, value = nil)
  if min_size <= array.length
    return array
  else
    difference = min_size - array.length
    while difference > 0
      array << value
      difference -= 1
    end
    return array
  end
end


def pad(array, min_size, value = nil)
  new_array = []
  array.each do |item|
    new_array << item
  end
  if min_size <= array.length
    return new_array
  else
    difference = min_size - array.length
    while difference > 0
      new_array << value
      difference -= 1
    end
    return new_array
  end
end
=end

# 3. Refactored Solution

def pad!(array, min_size, value = nil)
  if min_size <= array.length
    return array
  else
    difference = min_size - array.length
    array.fill(value, array.length, difference)
  end
end

def pad(array, min_size, value = nil)
  new_array = []
  array.each do |item|
    new_array << item
  end
  if min_size <= array.length
    return new_array
  else
    difference = min_size - array.length
    new_array.fill(value, array.length, difference)
    return new_array
  end
end




# 4. Reflection

=begin

Were you successful in breaking the problem down into small steps?

-Yes! It is a very helpful part of the process to nail down the "fuzzy" version before getting into the code.

Once you had written your pseudocode, were you able to easily translate it into code? What difficulties and successes did you have?

-We figured it out mostly easily, just has to remember to include code to return the oriinal array if the value was less than the array length.


Was your initial solution successful at passing the tests? If so, why do you think that is? If not, what were the errors you encountered and what did you do to resolve them?

-We had initial failures with the destructive method, as we originally tired to declare the new_array that is spit out as = to the original. This returned an array with the same Object ID as the original when no additional items were padded in, so instead we created the new_array by iterating through the input and copying values over one-by-one to create a clone.

When you refactored, did you find any existing methods in Ruby to clean up your code?

-Yup; .fill was our helper in refactoring.

How readable is your solution? Did you and your pair choose descriptive variable names?

-Very and Yes - take a look at the code, it is tabbed so nicely!

What is the difference between destructive and non-destructive methods in your own words?

-The non-destructive method operates will return a new object which has been operated on, while the destructive one changes the contents of the object being operated on. More specifically, the Object ID of the result of a destructive method is the same as the original object, while the non-destructive method returns one with a different ID.

=end
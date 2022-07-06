# Die Class 2: Arbitrary Symbols


# I worked on this challenge [by myself].
# I spent [2] hours on this challenge.

# Pseudocode

# Input: An array of strings.
# Output: A random string.
# Steps: Create a die that takes an array of strings as an input.
# Create a method that randomly returns one of the strings when
# Create an ArgumentError if the array is empty.


# Initial Solution

class Die
  def initialize(sides)
    if sides.empty?
      raise ArgumentError.new('Supply at least one side.')
    end
    @sides = sides
  end

  def sides
    @sides.length
  end

  def roll
    @sides.sample
  end
end

# Refactored Solution

class Die
  ERROR = 'Supply at least one side.'

  def initialize(sides)
    raise ArgumentError.new(ERROR) if sides.empty?
    @sides = sides
  end

  def sides
    @sides.length
  end

  def roll
    @sides.sample
  end
end

# Reflection

# What were the main differences between this die class and the last one you created in terms of implementation? Did you need to change much logic to get this to work?

#  This die took in an array and the last die took in integers.  I think this challenge was pretty similar to the last one the only difference is for sides we had to determine the length and insead of rand for roll I had to use sample because I was working with an array.

# What does this exercise teach you about making code that is easily changeable or modifiable?

# This exercise taught me that making code that is changeable or modifiable is is important, but it needs to be done in away that if you change the value of one variable that the variable is changed throughout the whole program.  Creating good variables and constants when necessary helps making changes to code simpler later.

# What new methods did you learn when working on this challenge, if any?

#  I did not use any new methods in this challenge.  I have used length which determines the length of an array and I have also used sample which randomly selects elements of an array.  I did create a constant for my argument error which I think helped make my code a little more readable and easier to change if that error message needed to be amended.

# What concepts about classes were you able to solidify in this challenge?

# I was able to solidify how a class works.  For some reason classes have proven very abstract for me and I think that I need a lot of practice to really get them down.  This was helpful in working off of what we already learned last week.
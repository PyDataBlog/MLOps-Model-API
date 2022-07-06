# Die Class 2: Arbitrary Symbols


# I worked on this challenge [by myself, with: ].
# I spent [#] hours on this challenge.

# Pseudocode

# Input: array of strings
# Output: random selection from the array
# Steps: initialize the die with a non-empty array
# define a method that finds the number of sides (strings in the array)
# define a method that rolls and returns a random string from the die array


# Initial Solution

class Die
  def initialize(labels)
    if labels == []
      raise ArgumentError.new("Please enter a valid array of strings to set up the die")
    end
  @sides = labels.length
  @labels = labels
  end

  def sides
  @sides
  end

  def roll
    p @labels[rand(@sides)]
  end
end


# dice = Die.new(["3"])
# #p dice
# #p dice.sides
# #dice.roll
# Refactored Solution



#nothing to refactor




# # Reflection
# What were the main differences between this die class and the last one you created in terms of implementation? Did you need to change much logic to get this to work?
# # Very similiar in implementation, the biggest things that changed were:
# 1) the syntax for the error check in intialize now looking for an empty string instead of a particular value
# 2) needing to declare another instance variable for the labels themselves so we could call them again later during the roll
# 3) changing the roll method to run rand instead the index call for the @labels array
# What does this exercise teach you about making code that is easily changeable or modifiable?
# super helpful that the solution from the last exercise was easily changeable as it took far less time this week to just build on top of it.
# What new methods did you learn when working on this challenge, if any? none
# What concepts about classes were you able to solidify in this challenge? usage of the instance variables to make available information to class methods.
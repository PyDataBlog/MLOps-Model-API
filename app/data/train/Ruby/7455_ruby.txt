# Die Class 2: Arbitrary Symbols


# I worked on this challenge [by myself, with: ].
# I spent [#] hours on this challenge.

# Pseudocode

# Input: An array of strings
# Output: One of the strings from the array, randomly generated
# Steps:

# Create ArgumentError for if an empty array is passed through the program
# Create instance variables for sides and roll
# Allow for sides to be both integers and strings
# Tell the method to return a random roll of either integers or strings when run 



# Initial Solution

class Die
  def initialize(labels)

      @labels = labels
      @roll = roll
      @sides = sides
    if labels == []
      raise ArgumentError.new ("Array cannot be empty. Input integers/strings")
    end
  end

  def labels
    @labels
  end
  
    def sides
      @sides = @labels.length
    end
  
  def roll
    return @labels.shuffle[0]
  end
end

d = Die.new(['meow', 'woof', 'C'])
p d.sides
p d.roll
p d.labels


# Refactored Solution


class Die
  def initialize(faces)

      @faces = faces
      @roll = roll
      @sides = sides
    if faces == []
      raise ArgumentError.new ("Array cannot be empty. Input integers/strings")
    end
  end

  def faces
    @faces
  end
  
    def sides
      @sides = @faces.length
    end
  
  def roll
    @faces.shuffle[0]
  end
end

d = Die.new(['meow', 'woof', 'C'])
p d.sides
p d.roll
p d.faces

#There isn't really much here that can be refactored, at least not from what I can discern




# Reflection

# What were the main differences between this die class and the last one you created in terms of implementation? Did you need to change much logic to get this to work?

# I was not sure how to implement this at first. The outline of code that was provided in the GitHub repository did not specify including a variable called labels, but that was the only way I could forsee it working. So, this Class is working with three variables, not two. Also, the argument and the number of sides in this program are not the same, as they were in the first class, so it took awhile to figure that out and get it working the way it should.

# What does this exercise teach you about making code that is easily changeable or modifiable? 

# Having precise variable names helps a lot when figuring out what exactly each variable is doing in the code. The name sides is somewhat misleading, as is the name labels. I wish it were something like 'faces,' which is why I changed it in my refactor. To me, that makes what that variable actually represents more clear. This code was relatively easy to change/modify because of its simplicity and its clarity.

# What new methods did you learn when working on this challenge, if any?

# I did not learn any new methods when working on this challenge.

# What concepts about classes were you able to solidify in this challenge?

# I am getting a better handle on what classes do and how they function. I do not think I would be able to write one with perfect syntax without consulting a reference first, but I am sure that this will come with time.
#Attr Methods

# I worked on this challenge by myself.

# I spent [#] hours on this challenge.

# Pseudocode

# Input:
# Output:
# Steps:

class NameData

end


class Greetings

end

# Release 1

#What are these methods doing?
# These methods set a name, age, and occupation, then they allow those instance variables to change by calling a new method to update the value of those instance variables.

#How are they modifying or returning the value of instance variables?
## They pass an argument into a new method that updates the value of the instance variables.


# Release 2

# What changed between the last release and this release?
## Now the @age variable is readable without needing to explicitly write a method that exposes it.
#What was replaced?
## The what_is_age() method was replaced with attr_reader.

#Is this code simpler than the last?
## Yes because now you can call class.age to return the age of the person.

# Release 3
#What changed between the last release and this release?
## Now the @age variable can be writeable without needing to explicitly write a method that allows it to be changed. If you set it equal to a new value, it'll change.

#What was replaced?
## The change_my_age method is no longer necessary.

#Is this code simpler than the last?
## Sure is. Now we can just set the age equal to a new value and it'll change.

# Release 4
## see release_4.rb

# Release 5
##
class NameData

  attr_accessor :name

  def initialize
    @name = 'Ryan'
  end
  def print_name
    "#{name}"
  end
end

class Greetings
  def hello
    namedata = NameData.new
    name = namedata.print_name
    puts "Hello #{name}! How wonderful to see you today."
  end
end


greet = Greetings.new
greet.hello # puts "Hello Student! How wonderful to see you today."



# Reflection
##What is a reader method?
## It allows you to access the value of a variable from outside the class
##What is a writer method?
## It allows you to update the value of a variable from outside the class
#What do the attr methods do for you?
## It's a shorthand way of letting you update the method from elsewhere
#Should you always use an accessor to cover your bases? Why or why not?
## Not necessarily because you don't always want to let other methods and classes update the value of your objects
#What is confusing to you about these methods?
## The shorthand notation took some getting used to.
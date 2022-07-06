#Attr Methods

# I worked on this challenge [by myself, with:]

# I spent [1.5] hours on this challenge.

class NameData

  attr_accessor :name

  def name
    @name= "Marcel"
  end

end


class Greetings

attr_reader :name

  def initialize
    @namedata= NameData.new
  end

  def hello
    puts @namedata.name + "!"
    puts "How goes?"
  end
end

greet = Greetings.new
greet.hello

# Reflection
=begin
6.4
Release 1
What are these methods doing?
Basically they are modifying the values.
How are they modifying or returning the value of instance variables?
An instance variable will always be the last value it was set as.  They are using a method to do each one; age, name, occupation.
Release 2
What changed between the last release and this release?
What was replaced?
Is this code simpler than the last?
#what_is_age was replaced by attr_reader :age.  This is much simpler and less repetitive.

Release 3
What changed between the last release and this release?
What was replaced?
Is this code simpler than the last?
attr_writer :age added and #change_my_age removed.  Now we can change :age from outside of the class if desired.  This is simpler code.

Release 5
What is a reader method?
Returns a value outside of the class but is read only
What is a writer method?
You can change the value of a attr_writer from outside of the class but you can’t read it.
What do the attr methods do for you?
Allow instance variables to be carried over classes.
Should you always use an accessor to cover your bases? Why or Why not?
No.  For security and debugging always using the accessor class can cause problems.
What is confusing to you about these methods?
Once I understood the perspective these attributes have it all came to me.
=end
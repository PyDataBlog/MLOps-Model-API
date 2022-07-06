# METHODS: More about methods 
#
# You can make methods, too! 
# Method names should begin with a lowercase letter.


def is_this_a_method?
  "Yes, this is a method!" 
  # "Yes, this is a method!" is an object. 
  # Because this is the last object in the method, its address will be returned.
end

is_this_a_method? # => "Yes, this is a method!"

# methods can pass value also...

def writeln(string)
  p(string)
end

writeln("This method accepts a string") # => "This method accepts a string"


# TIP: in your editor type "def" and hit the tab key...you should see this...
def method_name
	
end

def town
 whatev 
end
# Note: Methods should be defined before calling them, otherwise Ruby will raise an exception for undefined method

# >> "This method accepts a string"

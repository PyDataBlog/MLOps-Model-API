# U2.W6: Testing Assert Statements

# I worked on this challenge with Mark McQui!!en and Max Lock.


# 2. Review the simple assert statement

def assert
  raise "Assertion failed!" unless yield
end

name = "bettysue"
assert { name == "bettysue" }
assert { name == "billybob" }

# 2. Pseudocode what happens when the code above runs

def assert
  raise "Assertion failed!" unless yield
end

# Defining an assert method and raising an error if it is not equal

name = "bettysue"

# Defining the variable (name)

assert { name == "bettysue" }
assert { name == "billybob" }

# Testing the assert method to see if the name is equal to the input (not sure if it is considered input)

# We got a runtime error because "billybob" is not equal to the variable name, hence the assertion failed. It is referencing the irb line 19 which sets the assertion statement. It then references line 24 pointing out where the assertion statement was prompted.

# 3. Copy your selected challenge here
class GuessingGame

	def initialize(answer)
  		@answer = answer
  end

	def guess(guess)
		@guess = guess
		if @guess == @answer 
		return :correct
		elsif @guess > @answer
		return :high
		else return :low
		end
	end

	def solved?
			return @guess == @answer 
	end
end


# 4. Convert your driver test code from that challenge into Assert Statements
def assert
    raise "Assertion Failed!" unless yield
end

test1 = GuessingGame.instance_method(:initialize).arity
assert { test1 == 1}
assert { test1 == 3}

test2 = GuessingGame.method_defined?(:guess)
assert { test2 == true}
assert { test2 == false}


test3 = GuessingGame.method_defined?(:solved?)
assert { test3 == true}
assert { test3 == false}


# 5. Reflection
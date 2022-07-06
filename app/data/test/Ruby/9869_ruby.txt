# Calculate the mode Pairing Challenge

# I worked on this challenge [by myself, with: ]

# I spent [] hours on this challenge.

# Complete each step below according to the challenge directions and
# include it in this file. Also make sure everything that isn't code
# is commented.



# 0. Pseudocode
# What is the input? Array of number or strings
# What is the output? An array of the most frequent number, or single element array.
# What are the steps needed to solve the problem?
# 1.define a method called mode
# 2.create a hash
# 3.Iterate over the array to push elements into the hash, elements being the keys and     the value as the counter
# 4.Look for the max counter number in the hash
# 5. Output the key in an array


# 1. Initial Solution
# def mode(array)
#   frequency = Hash.new(0)
#   array.each{|element|frequency[element] +=1}
#   array2=[]
#   frequency.each do |key,value|
#     if value == frequency.values.max
#       array2 << key
#     end
#   end
#   array2
# end

# h = Hash[arr.collect { |v| [v, f(v)] }]
# 3. Refactored Solution
def mode(array)
  frequency = {}
  array.each{|element| frequency[element] = array.count(element)}
  modes = frequency.select {|k,v| v == frequency.values.max}
  modes.keys
end

# Reflection

# Which data structure did you and your pair decide to implement and why?

# We used both arrays and hashes since we need to take in an array and output an array, but hashes made the most sense for storing a count associated with each element.

# Were you more successful breaking this problem down into implementable pseudocode than the last with a pair?

# We were able to break the problem down into smaller pieces, but it was still difficult since we weren't sure what would work.

# What issues/successes did you run into when translating your pseudocode to code?

# We translated the pseudocode to real code line by line, but the implementation changed a little as we went on.

# What methods did you use to iterate through the content? Did you find any good ones when you were refactoring? Were they difficult to implement?

# We originally just used the each method, but later used the select method after playing around with the partition method. 

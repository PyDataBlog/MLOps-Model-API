# Factorial

# I worked on this challenge [by myself, with: Emily Mosowski ].


# Your Solution Below


# Your code goes here
def factorial(number)
  sum = 1
  sum.upto(number) { |i| sum *= i}
  sum
end

puts factorial(10)

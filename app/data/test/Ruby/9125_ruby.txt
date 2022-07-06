# Full name greeting: Write a program that asks for a person's first name, then middle, then last. Finally, it should greet the person using their full name.

puts "Hi, what's your first name?"
first_name = gets.chomp
puts "Hi, what's your middle name?"
middle_name = gets.chomp
puts "Hi, what's your last name?"
last_name = gets.chomp
puts "Your name is " + first_name + " " + middle_name + " " + last_name + "."

# Bigger, better favorite number: Write a program that asks for a person's favorite number. Have your program add 1 to the number, and then suggest the result as a bigger and better number.

puts "What's your favorite number?"
number = gets.chomp
number2 = number.to_i + 1
puts "How about " + number2.to_s + " ? It is a better number."

# Links to small exercises
https://github.com/mirascarvalone/phase-0/blob/master/week-4/math/my_solution.rb
https://github.com/mirascarvalone/phase-0/blob/master/week-4/address/my_solution.rb


#Reflection

#How do you define a local variable?
  # You can define a local variable by defining it (as a lowercase word with no spaces), then typing enter, and then defining it as a number or string.
#How do you define a method?
  # You define a method by first typing def, then the name of the method, and then in parenthesis indicate its arguments. Then define the argument below and make sure it ends with "end."
#What is the difference between a local variable and a method?
  # A local variable is a way to store a value and reference it later within a block of code. A method is a pre-defined block of code that acts on a set of arguments (or inputs) and produces an output based on those.
#How do you run a ruby program from the command line?
  # To run a ruby program from the command line, simply type ruby and then the name of the program (including the .rb extension).
#How do you run an RSpec file from the command line?
# To run an RSpec file from the command line, type rspec and then the name of the program.
# => What was confusing about this material? What made sense?
# I thought it was pretty intuitive and liked having all of the examples. The more examples I work through, the more I understand.
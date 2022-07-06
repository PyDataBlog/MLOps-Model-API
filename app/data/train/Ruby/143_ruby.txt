## Your Names
# 1) Michael Yao
# 2) Benjamin Heidebrink

# We spent [1.25] hours on this challenge.

# Bakery Serving Size portion calculator.

def serving_size_calc(item_to_make, num_of_ingredients)
  library = {"cookie" => 1, "cake" =>  5, "pie" => 7}
   
  raise ArgumentError.new("#{item_to_make} is not a valid input") unless library[item_to_make]
  # fail ArgumentError, "#{item_to_make} is not a valid input" unless library.key?(item_to_make)
  serving_size = library[item_to_make]
  remaining_ingredients = num_of_ingredients % serving_size

  baking_plan = "Calculations complete: Make #{num_of_ingredients / serving_size} of #{item_to_make}"
  
  
   return baking_plan if remaining_ingredients.zero?
   baking_plan + ", you have #{remaining_ingredients} leftover ingredients. Suggested baking items: #{leftovers(remaining_ingredients)}"
end


def leftovers(remaining_ingredients)
  cakes=remaining_ingredients/5
  cookies=remaining_ingredients%5
  
  "#{cakes} cakes and #{cookies} cookies."
end

p serving_size_calc("pie", 7)
p serving_size_calc("pie", 8)
p serving_size_calc("cake", 5)
p serving_size_calc("cake", 7)
p serving_size_calc("cookie", 1)
p serving_size_calc("cookie", 10)
p serving_size_calc("THIS IS AN ERROR", 5)

#  Reflection

# What did you learn about making code readable by working on this challenge?
# It's preeettty important. Also we learned some new methods/syntax to make it more readable and truthy/falsey

# Did you learn any new methods? What did you learn about them?
# .zero? .key? .nil? .values_at

# What did you learn about accessing data in hashes? 
# .values_at returns an array, .key checks for keys., if we show one argument it will return an array of hash/key
# What concepts were solidified when working through this challenge?
# refactoring, some syntax
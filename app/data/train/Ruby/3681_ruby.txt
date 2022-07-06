# Pseudocode
# First, put names in an array separated with commas (I'll do this in excel). It will look like:
#     array = ["name 1","name 2"] etc.
# I will remove the last 5 elements from the array and that will be the first accountability group. I will repeat this process until there are 7 or fewer elements in the list. Then I will apply the following logic:
# IF length = 7, split up into one group of 3, one of 4
# IF length = 6, split up into 2 groups of 3
# IF length <6, return remaining amount as its own group


# Initial Solution

roster = ['Jack Abernethy', 'Mohammad Amin', 'Zollie Barnes', 'Reuben Brandt', 'Dana Breen', 'Breton Burnett', 'Saundra Vanessa Castaneda', 'Luis De Castro', 'Nicolette Chambers', 'Kerry Choy', 'Nick Davies', 'KB DiAngelo', 'Adrian Diaz', 'David Diaz', 'Bob Dorff', 'Michael Du', 'Paul Dynowski', 'Jenna Espezua', 'Sean Fleming', 'Marcel Haesok', 'Albert Hahn', 'Arthur Head', 'Jonathan Huang', 'Thomas Huang', 'Alex Jamar', 'Kevin Jones', 'Steven Jones', 'Coleby Kent', 'Caroline Kenworthy', 'Calvin Lang', 'Yi Lu', 'David Ma', 'Sean Massih', 'Tom McHenry', 'Alex Mitzman', 'Lydia Nash', 'Brenda Nguyen', 'Matthew Oppenheimer', 'Mason Pierce', 'Joe Plonsker', 'Mira Scarvalone', 'Joseph Scott', 'Chris Shahin', 'Benjamin Shpringer', 'Lindsey Stevenson', 'Phil Thomas', 'Gary Tso', 'Ting Wang', 'Monique Williamson', 'Regina Wong', 'Hanah Yendler']


def groups(array)
  array.shuffle!

  while array.length > 7
    puts "Group: "
    puts array.pop(5)
  end

  if array.length == 7
    puts "Group: "
    puts array[0,3]
    puts "Group: "
    puts array[3,6]
  end

  if array.length == 6
    puts "Group: "
    puts array[0,3]
    puts "Group: "
    puts array[3,5]
  end

  if array.length <= 5
    puts "Group: "
    puts array
  end
end

groups(roster)

# Refactored Solution
def groups(array)
  array.shuffle!

  if array.length % 5 != 1 && array.length % 5 != 2
    print array.each_slice(5).to_a
  elsif array.length % 5 ==1
    print array.pop(3)
    print array.each_slice(5).to_a
  elsif array.length % 5 ==2
    print array.pop(4)
    print array.each_slice(5).to_a
  end
end

groups(roster)

# Reflection
# What was the most interesting and most difficult part of this challenge?
#   This challenge was difficult in that it had less structure than others and we could kind of do whatever we wanted. I did enjoy thinking through the math portion and how different length arrays would need to be treated differently so we didn't have 1 or 2 people in a group (ironically, this happened to me the first week of phase 0 :) ). One of the most difficult parts for me was that I wanted to give the groups names, but couldn't find a way to make an array name dynamic.
# Do you feel you are improving in your ability to write pseudocode and break the problem down?
#   Yes, although I often find it helpful to start coding certain parts of my psuedocode before finishing it to see what works and what doesn't.
# Was your approach for automating this task a good solution? What could have made it even better?
  # I think the refractoring code was much cleaner. Overall though, my solution does accomodate arrays of any size and will change the order of the elements, which assures random groups.
# What data structure did you decide to store the accountability groups in and why?
  # I put them in an array because that seemed to make sense since we just want a grouping of elements.
# What did you learn in the process of refactoring your initial solution? Did you learn any new Ruby methods?
#   Yes, I learned the .each_slice method which is a lot cleaner. Also I learned the .to_a method.
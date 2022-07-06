#4.2 Reaction Paper#

"puts" returns nil, and then prints the outcome to the console with a line break after it.
An integer is any number that doesn't end with a decimal point, like 89 or 3. Floats end with a decimal point, e.g. 1.2, 7.9999999999 or 45.0.
Integer division is the kind we learn in school, float division rounds the output up or down (usually down) to an integer. I would explain that computers are dumb, they need two categories to understand numbers, and they cannot conceptualize that all numbers can be reprented as both integers and floats. And that all operations can change the value of the number, but the computer won't change what kind of number it is unless it's specifically told.
___
```ruby
hours_in_year = 24*365.25
puts "There are #{hours_in_year} hours in a year"
minutes_in_decade= 60*24*365.25*10
print "There are #{minutes_in_decade} minutes in a decade"
```
___
My code for the following exercises:
*[Defining Variables](defining_variables.rb)
*[Simple String Methods](simple-string.rb)
*[Local Variables and Basic Arithmetical Operations](basic-math.rb)
___

*How does Ruby handle addition, subtraction, multiplication, and division of numbers?*
  Ruby stores integers and floats(numbers with decimals) as different categories. One can add, subtract, and multiply integers and receive integers, and floats as floats. Division, on the other hand, returns numbers that would be characterized as floats when two integers are divided. Ruby rounds quotients unless either number is changed or re-represented as a float.

*What are strings? Why and when would you use them?*
  Ruby encodes information in different ways, different kinds of information merits different kinds of methods. And it's easier to group methods conceptually based on what kind of information they act upon. Strings are a grouping of characters, including integers, spaces, punctuation, etc. They can be broken into substrings and searched for their information. An address in a database could be stored as a string, a list of addresses with the same zip code would not. My name on Facebook could be stored as a string, the number of times I've looked at Elon Musk's profile could not.

*What are local variables? Why and when would you use them?*
  Their names start with lower cases or underscores, they act as temporary placeholders for the information being manipulated within the steps of a method. You cannot access (in trying to call or use their values) them outside of the method in which they "live".

*How was this challenge? Did you get a good review of some of the basics?*
  I have some experience with Ruby, so it wasn't too hard. And it felt good compared to git, with which I had no experience. But I want to guard myself against an illusion of competence in my understanding of Ruby. Building a poor understanding of a big thing based on poor understanding of basic concepts is 1) easy to prevent/change and 2) super common.
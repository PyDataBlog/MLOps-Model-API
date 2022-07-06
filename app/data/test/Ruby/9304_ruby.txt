# OO Basics: Student


# I worked on this challenge with Karen Ball.
# This challenge took me [2.5] hours.


# Pseudocode
=begin
Create array of students with 5 students and array of 5 test scores

Average Scores method
Add up scores in score array
Divide sum by length of array

Letter Grade method
Take average and find corresponding letter grade

Linear Search
iterate over the array
if the first name is present, return the position of the first name
if it is not present return -1
=end

# Initial Solution

# class Student

class Student
  attr_accessor :scores, :first_name

  def initialize(first_name, scores)
      @first_name = first_name
      @scores = scores
  end
 
  def average
   @avg = @scores.reduce(:+) / @scores.length 
  end
  
  def letter_grade
    case @avg
    when (90..100)
      'A'
    when (80...90)
      'B'
    when (70...80)
      'C'
    when (60...70)
      'D'
    when (0...60)
      'F'
    end
  end
  
  def linear_search(array, name)
    array.each_with_index do |student, index|
      if @first_name == name
        return index
      else
        return -1
      end
    end
  end
end

#Array of Students 

student_1 = Student.new("Alex", [100, 100, 100, 0, 100])
student_2 = Student.new("Bob", [90, 90, 100, 80, 96])
student_3 = Student.new("Mary", [80, 80, 84, 82, 88])
student_4 = Student.new("Joe", [70, 60, 72, 66, 80])
student_5 = Student.new("Jane", [98, 96, 92, 100, 100])
students = [student_1, student_2, student_3, student_4, student_5]


# Refactored Solution




# DRIVER TESTS GO BELOW THIS LINE
# Initial Tests:

p students[0].first_name == "Alex"
p students[0].scores.length == 5
p students[0].scores[0] == students[0].scores[4]
p students[0].scores[3] == 0


# Additional Tests 1:

p students[0].average == 80
p students[0].letter_grade == 'B'

# Additional Tests 2:

p students[0].linear_search(students, "Alex") == 0
p students[0].linear_search(students, "NOT A STUDENT") == -1


# Reflection
=begin
What concepts did you review in this challenge?

In this challenge we reviewed how to create a class and write attribute accessors.
In addition to that we reviewed case statements and how to properly write them, and how to write
methods within a class and access the local variables between methods.

What is still confusing to you about Ruby?

General familiarity with certain methods and why things work is still a bit confusing to me about Ruby. We tried to
put a > sign in our case statement and could not figure out why it was not working until we learned that in case statements
we must use a range.

What are you going to study to get more prepared for Phase 1?	

To get more prepared for Phase 1 I plan to continue reviewing how to write a class and methods to access elements across it.
=end
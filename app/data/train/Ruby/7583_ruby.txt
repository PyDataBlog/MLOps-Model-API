# Introduction to Inheritance

# I worked on this challenge [by myself, with: ].


# Pseudocode

#Attributes
#city - Local
#students - both
#name - Global (Fiery Skippers)
#phase 0 start date - Global
#immersive start date - Global
#graduation date - Global
#email_list - both


# Initial Solution
require 'date'
class GlobalCohort
  attr_accessor :p0_start_date, :name
  attr_writer :email_list

  def initialize(name, phase0_start_date)
    @name = name
    @p0_start_date = phase0_start_date
    @students = []
    @email_list = {}
  end
  
  def immersive
    @p0_start_date >> 2
  end
  
  def graduation
    @p0_start_date >> 4
  end
  
  def add_students(*students)
    @students << students
  end

  def list_serve
    @email_list
  end

end

class LocalCohort < GlobalCohort
  attr_accessor :city
  def initialize(city)
    @city = city
  end
  
end

globe = GlobalCohort.new("Fiery Skippers", Date.new(2016, 2, 8))

globe.email_list = {"ted": "bogin@gmail"}


nyc = LocalCohort.new("New York City")
nyc.name = "Fiery Skippers" 
p nyc.name
# Refactored Solution



# Reflection

#I initially thought that subclasses inherited the values set in the superclass, instead of only the methods. Once I realized my mistake,
#inheritance becamem pretty straightforward.
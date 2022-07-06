#Step definitions for the Accumulator class

Given /^I have an initial value of (\d+)$/ do |arg1|
  $starting_value = arg1.to_i
end

#TODO: These two steps have common code. should be factored out
Given /^I have constructed a new Accumulator with the parameter (\d+)$/ do |arg1|
  $accumulator = Accumulator.new(arg1.to_i)
end
When /^I construct a new Accumulator with parameter (\d+)$/ do |arg1|
  $accumulator = Accumulator.new(arg1.to_i)
end

Then /^the value of the Accumulator should be (\d+)$/ do |arg1|
  $accumulator.get_value == arg1.to_i
end

Given /^I have constructed an Accumulator with the parameter (\d+)$/ do |arg1|
  $accumulator = Accumulator.new(arg1.to_i)
end

When /^I construct a new Accumulator with the starting value$/ do
  $accumulator = Accumulator.new($starting_value.to_i)
end

Then /^the value of the Accumulator should be the starting value$/ do
  $accumulator.get_value == $starting_value.to_i
end

When /^I add (\d+) to the Accumulator$/ do |arg1|
  $accumulator.add(arg1.to_i)
end

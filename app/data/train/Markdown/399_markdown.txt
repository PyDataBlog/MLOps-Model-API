# Elevator of the Americas

Welcome! This is a small project that describes an elevator in code - including dispatching and interacting with the elevator.

Desired requirements:

  * The elevator bank must have at least 3 elevators
  * The elevator bank must have a way to have elevators dispatched to certain floors
  * The elevators must follow rules as to which one is dispatched to which floor.
  * When we instantiate an elevator bank, let's make sure the elevators are on random floors so we have a realistic experience.

# Example

```ruby
bank = ElevatorOfAmericas::ElevatorBank.new
bank.dispatch_to(3, :down) # => Figures out which elevator can be dispatched to the elevator and sends it there for a certain direction
bank.elevators # => Returns all elevators and where they are and perhaps state (idle, moving_up, moving_down)
```

# What to do

The spec files are mostly empty or missing, we'd like to fill them out to ensure that we have a good mix of tests of different types: unit, integration, acceptance, etc. The goal is not only to write some tests, but also to have a good conversation about what good testing looks like: what's the appropriate amount and why?

Although there are no time limits, we don't want to take up too much of your time, a few hours should be sufficient. Focus on quality!

# Authors

Robert Ross, Stafford Brooke, Phillip Baker

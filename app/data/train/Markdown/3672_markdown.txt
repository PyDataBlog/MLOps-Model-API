## Precision
Calculate the precision of anything that can be converted into a decimal.

```ruby
irb> Precision::Calculator.new(BigDecimal("12.12")).calculate
=> 2

irb> Precision::Calculator.new("12.1234").calculate
=> 4

irb> Precision::Calculator.new(12.123).calculate
=> 3

irb> Precision::Calculator.new(nil).calculate
=> 0

# #calculate is aliased to #to_i
irb> precision = Precision::Calculator.new(1)
irb> precision.to_i == precision.calculate
=> true
```

### Why might this be helpful?


Create the `precision` option argument using the current value of an attribute:

```ruby
<%= number_to_currency @model.n, precision: Precision::Calculator.new(@model.n).to_i %>
```

## Installation


    gem 'precision'

## Contributions

- Created by [@barelyknown](https://twitter.com/barelyknown)
- You?

# Minitest::Mock::Easily

use MiniTest::Mock with easier API

## Installation

Add this line to your application's Gemfile:

    gem 'minitest-mock-easily'

And then execute:

    $ bundle

Or install it yourself as:

    $ gem install minitest-mock-easily

## Usage

```
include MockEasily

m = mock do
  expect :hello, 'hello'
  expect :world, 'world'
end

assert_equal 'hello', m.hello
assert_equal 'world', m.world
```

## Contributing

1. Fork it ( http://github.com/<my-github-username>/minitest-mock-easily/fork )
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request

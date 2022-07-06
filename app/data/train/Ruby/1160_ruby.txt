require_relative File.join 'support', 'coverage'
require_relative File.join '..', 'lib', 'arukamo'

subject = Object.new

fail unless subject.aru? === true
fail unless subject.nai? === false

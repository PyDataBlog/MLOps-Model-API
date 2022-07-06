require 'test/unit'

class FizzbuzzTest < Test::Unit::TestCase

  # Called before every test method runs. Can be used
  # to set up fixture information.
  def setup
    # Do nothing
  end

  # Called after every test method runs. Can be used to tear
  # down fixture information.

  def teardown
    # Do nothing
  end

  # Fake test
  def test_fail

    expected = 1
    actual =1
    assert_same expected, actual
    # fail('Not implemented')
  end

  def test_fizzbuzz
    require './fizzbuzz.d/rb-fizzbuzz'
    res=[]
    1.upto(100).each {|e|
      next if fizbuz e, res
      next if fiz e, res
      next if buz e, res
      res << e
    }
    assert_same 100, res.length
  end
end
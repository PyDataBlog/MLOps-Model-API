require "rspec"

extend RSpec::Matchers

describe "Bad Unit Tests" do

  it "should show an example of forced passes" do
    # Doesn't matter what happens here, since assertion always passes
    expect(true).to be true
  end

end

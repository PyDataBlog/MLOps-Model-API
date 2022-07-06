# SimpleBdd::Examples

This is an example of how to use the simple_bdd library alongside mixing in modules depending on the type of test being run.

It's test framework agnostic, but here this is using RSpec. The strings sent to the methods 'Given', 'When', 'Then' and 'And' are translated into a method which is called in the current scope. 

    it "has this amazing new feature" do
      Given "Some state in the application"
      When "this state transition is applied"
      Then "this side affect occurs"
    end
    
In the RSpec config, modules are loaded depending on what tags are on the example.  Here different tags are applied to the same test, effectively duplicating the test but with different tags applied each time.

	[:integration, :journey].each do |type|
  	  describe "Example", group: type do
    	it "has this amazing new feature" do
   		  Given "Some state in the application"
		  When "this state transition is applied"
		  Then "this side affect occurs"
    	end
  	  end
	end
	
Then the configuration loads different helper modules depending on the tags.

    require 'simple_bdd'
	require 'journey_helpers'
	require 'integration_helpers'

	RSpec.configure do |config|
  	  config.include SimpleBdd
  	  config.include JourneyHelpers, group: :journey
  	  config.include IntegrationHelpers, group: :integration
	end

Then the JourneyHelpers and IntegrationHelpers modules have the methods that each tag requires such as:

	module IntegrationHelpers
  	  def some_state_in_the_application
  	  end
  	  
	  def this_state_transition_is_applied
	  end

  	  def this_side_affect_occurs
  	  end
	end

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request

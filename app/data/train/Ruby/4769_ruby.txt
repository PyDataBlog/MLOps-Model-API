module ExtraJsonMatchers
  def have_json_value(object = nil)
    JsonSpec::Matchers::BeJsonEql.new(object.to_json)
  end
end

RSpec.configure do |config|
  config.include ExtraJsonMatchers, type: :view
end

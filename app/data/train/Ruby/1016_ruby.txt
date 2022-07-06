require 'simplecov'

require 'coveralls'
Coveralls.wear!

SimpleCov.start 'rails' do
  SimpleCov.formatters = [
    SimpleCov::Formatter::HTMLFormatter,
    Coveralls::SimpleCov::Formatter
  ]

  add_filter '/spec/'
end

require 'lita-wit'

require 'lita/rspec'
require 'lita_config'

# A compatibility mode is provided for older plugins upgrading from Lita 3. Since this plugin
# was generated with Lita 4, the compatibility mode should be left disabled.
Lita.version_3_compatibility_mode = false

require 'vcr'
require 'webmock/rspec'

VCR.configure do |config|
  config.cassette_library_dir = 'spec/support/vcr_cassettes'
  config.ignore_localhost = true
  config.hook_into :webmock
  config.configure_rspec_metadata!
end

SESSION_ID = 'unique-1234'

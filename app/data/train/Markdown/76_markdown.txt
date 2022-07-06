# NS

Provides a Ruby wrapper for the Dutch NS API.

## Installation

Add this line to your application's Gemfile:

    gem 'ns'

And then execute:

    $ bundle

Or install it yourself as:

    $ gem install ns

## Usage

    ns_client = Ns::Client.new(api_key, api_password)
    available_stations = ns_client.get_stations
    advice = ns_client.get_travel_advice(available_stations[0], 
                                         available_stations[1])

Should return a travel advice based on the first and second available station.



## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Added some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request

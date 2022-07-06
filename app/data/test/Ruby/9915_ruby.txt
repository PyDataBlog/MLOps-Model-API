$LOAD_PATH.unshift File.expand_path('../../lib', __FILE__)
require 'coinbase/ledger'

require 'minitest'
require 'minitest/autorun'
require "dotenv"
require 'fakeweb'

BASE_URI = "https://coinbase.com/api/v1"

def fake method, path, body
  FakeWeb.register_uri method, "#{BASE_URI}#{path}", body: body.to_json
end

FakeWeb.allow_net_connect = false

fake :get, "/account/balance", {amount: "50.00000000", currency: 'BTC'}

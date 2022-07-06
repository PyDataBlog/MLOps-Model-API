require 'twitter'
# twitter_api_key: uRJ6tmBxZigeum9sOPZ4FqkEI
# twitter_secret_key: sUDrZGZ5maxObLu6LgmLxNYYdRGYFzLvI2VIgXRPh6sjBUuzYH
# twitter_access_token: 137897485-EY7bchwAjfJGnxbuIvaDFlPtMIBe8OPRpHn6P2zW
# twitter_access_token_secret: mYoXmkvE1F0wnP1zBs2U31gHsKI1fBb5RxUfy0f4fFnEc

# Twitter.api_key = ENV['twitter_api_key']
# Twitter.secret_key = ENV['twitter_secret_key']
# Twitter.access_token = ENV['twitter_access_token']
# Twitter.access_token_secret = ENV['twitter_access_token_secret']

Twitter::REST::Client.new do |config|
  config.consumer_key        = ENV['twitter_api_key']
  config.consumer_secret     = ENV['twitter_secret_key']
  config.access_token        = ENV['twitter_access_token']
  config.access_token_secret = ENV['twitter_access_token_secret']
end


#   config.consumer_key        = "uRJ6tmBxZigeum9sOPZ4FqkEI"
#   config.consumer_secret     = "sUDrZGZ5maxObLu6LgmLxNYYdRGYFzLvI2VIgXRPh6sjBUuzYH"
#   config.access_token        = "137897485-EY7bchwAjfJGnxbuIvaDFlPtMIBe8OPRpHn6P2zW"
#   config.access_token_secret = "mYoXmkvE1F0wnP1zBs2U31gHsKI1fBb5RxUfy0f4fFnEc"

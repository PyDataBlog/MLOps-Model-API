module CoderwallRuby
  class User
    include HTTParty
    base_uri 'https://coderwall.com'

    attr_accessor :username

    def initialize(username)
      self.username = username
    end

    def profile
      self.class.get "/#{ username }.json" 
    end

    def method_missing(name, *args, &block)
      if profile.has_key?(name.to_s)
        profile[name.to_s]
      else
        super
      end
    end

  end
end

module ArenaApi
  module Configuration
    class << self
      attr_accessor :email, :password, :workspace

      def configure
        yield self
      end
    end
  end
end

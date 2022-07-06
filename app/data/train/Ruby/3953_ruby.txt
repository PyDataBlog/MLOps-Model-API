require 'omniauth-oauth2'

module OmniAuth
  module Strategies
    class EyeEm < OmniAuth::Strategies::OAuth2
      option :client_options, {
        :site => 'https://api.eyeem.com/v2',
        :authorize_url => 'https://www.eyeem.com/oauth/authorize',
        :token_url => 'https://api.eyeem.com/v2/oauth/token'
      }

      option :name, "eyeem"

      uid { raw_info['user']['id'] }

      info do
        {
          :nickname => raw_info['user']['nickname'],
          :name => raw_info['user']['fullname'],
          :image => raw_info['user']['photoUrl'],
          :description => raw_info['user']['description'],
          :email => raw_info['user']['email']
        }
      end

      extra do
        { :raw_info => raw_info }
      end

      def raw_info
        @raw_info ||= access_token.get('users/me').parsed
      end
    end
  end
end

OmniAuth.config.add_camelization 'eyeem', 'EyeEm'
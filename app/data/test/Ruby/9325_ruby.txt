module Softlayer
  module Container
    module Utility
      module File
        class Descriptor < Softlayer::Entity
          attr_accessor :file_name
          attr_accessor :friendly_name
          attr_accessor :modify_date

          class Representer < Softlayer::Entity::Representer
            include Representable::Hash
            include Representable::Coercion
            property :file_name, type: String
            property :friendly_name, type: String
            property :modify_date, type: DateTime
          end
        end
      end
    end
  end
end

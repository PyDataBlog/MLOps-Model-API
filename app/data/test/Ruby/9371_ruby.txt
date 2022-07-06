module Softlayer
  class Network
    class Component
      module Network
        module Vlan
          class Trunk < Softlayer::Entity
            attr_accessor :network_component_id
            attr_accessor :network_vlan_id
            attr_accessor :network_component
            attr_accessor :network_vlan

            class Representer < Softlayer::Entity::Representer
              include Representable::Hash
              include Representable::Coercion
              property :network_component_id, type: Integer
              property :network_vlan_id, type: Integer
            end
          end
        end
      end
    end
  end
end

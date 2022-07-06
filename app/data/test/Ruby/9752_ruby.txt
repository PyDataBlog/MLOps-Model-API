module AsposeCellsCloud
  # 
  class DrawingOleObjects < BaseObject
    attr_accessor :ole_object_list, :link
    # attribute mapping from ruby-style variable name to JSON key
    def self.attribute_map
      {
        
        # 
        :'ole_object_list' => :'OleObjectList',
        
        # 
        :'link' => :'link'
        
      }
    end

    # attribute type
    def self.swagger_types
      {
        :'ole_object_list' => :'Array<LinkElement>',
        :'link' => :'Link'
        
      }
    end

    def initialize(attributes = {})
      return if !attributes.is_a?(Hash) || attributes.empty?

      # convert string to symbol for hash key
      attributes = attributes.inject({}){|memo,(k,v)| memo[k.to_sym] = v; memo}

      
      if attributes[:'OleObjectList']
        if (value = attributes[:'OleObjectList']).is_a?(Array)
          self.ole_object_list = value
        end
      end
      
      if attributes[:'link']
        self.link = attributes[:'link']
      end
      
    end

  end
end

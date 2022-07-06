class GetSharedLinkAccessRightsResult
  attr_accessor :access_rights

  # :internal => :external
  def self.attribute_map
  {
      :access_rights => :accessRights

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""

      if GetSharedLinkAccessRightsResult.attribute_map["access_rights".to_sym] != nil
        name = "access_rights".to_sym
        value = attributes["accessRights"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      end
  end

  def to_body
    body = {}
    GetSharedLinkAccessRightsResult.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end


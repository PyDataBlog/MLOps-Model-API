class SignaturePredefinedListSettingsInfo
  attr_accessor :name, :values, :default_value

  # :internal => :external
  def self.attribute_map
  {
      :name => :name, :values => :values, :default_value => :defaultValue

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""

      if SignaturePredefinedListSettingsInfo.attribute_map["name".to_sym] != nil
        name = "name".to_sym
        value = attributes["name"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if SignaturePredefinedListSettingsInfo.attribute_map["values".to_sym] != nil
        name = "values".to_sym
        value = attributes["values"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if SignaturePredefinedListSettingsInfo.attribute_map["default_value".to_sym] != nil
        name = "default_value".to_sym
        value = attributes["defaultValue"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      end
  end

  def to_body
    body = {}
    SignaturePredefinedListSettingsInfo.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end


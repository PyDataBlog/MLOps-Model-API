class TemplateField
  attr_accessor :page, :name, :type, :rect

  # :internal => :external
  def self.attribute_map
  {
      :page => :page, :name => :name, :type => :type, :rect => :rect

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""

      if TemplateField.attribute_map["page".to_sym] != nil
        name = "page".to_sym
        value = attributes["page"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if TemplateField.attribute_map["name".to_sym] != nil
        name = "name".to_sym
        value = attributes["name"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if TemplateField.attribute_map["type".to_sym] != nil
        name = "type".to_sym
        value = attributes["type"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      if TemplateField.attribute_map["rect".to_sym] != nil
        name = "rect".to_sym
        value = attributes["rect"]
        send("#{name}=", value) if self.respond_to?(name)
	      end
      end
  end

  def to_body
    body = {}
    TemplateField.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end


class Length

  def initialize value, unit
    @value = value
    @unit = unit
  end

  def value_in_mm
    @unit.convert_to_mm(@value)
  end

  def ==(other)
    return true if self.equal?(other)
    return false if other.nil?
    return false if other.class != self.class
    value_in_mm == other.value_in_mm
  end

  def hash
    37*(value_in_mm.hash)
  end

  def eql? other
    self == other
  end

end
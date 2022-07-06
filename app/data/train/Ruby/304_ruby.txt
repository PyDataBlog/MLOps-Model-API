require "point/version"

module Point
  autoload :Point, 'point/point'

  def self.sum(*args)
    x =  y = 0

    args.each do |point|
      x += point.x
      y += point.y
    end

    self::Point.new(x, y)
  end

end

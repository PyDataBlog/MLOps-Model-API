class Image < ActiveRecord::Base
  dragonfly_accessor :image

  attr_accessible :image, :title
end

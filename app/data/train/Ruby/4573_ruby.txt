class News < ActiveRecord::Base
  validates_presence_of :title
  validates_uniqueness_of :title
  validates_presence_of :category_id
  belongs_to :category
  has_many :images

end

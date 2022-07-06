class Post < ActiveRecord::Base
  has_many :comments
  has_many :tags, throug: :post_tags
  belongs_to :moderator

end

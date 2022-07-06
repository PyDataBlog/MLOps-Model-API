class User < ActiveRecord::Base
  # attr_accessor :name, :email, :favs
  has_secure_password
  has_many :favs, dependent: :destroy
  has_many :grids, dependent: :destroy

  def uploads
    Grid.where("user_id = ?", id)
  end
end

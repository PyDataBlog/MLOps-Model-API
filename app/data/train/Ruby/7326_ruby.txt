class User < ActiveRecord::Base
  include BCrypt

  has_many :favorites
  has_many :games, through: :favorites
  has_many :reviews

  validates :email, presence: true, uniqueness: true
  validates :hashed_password, presence: true

  def password
    @password ||= Password.new(hashed_password)
  end

  def password=(new_password)
    @password = Password.create(new_password)
    self.hashed_password = @password
  end

  def self.authenticate(email, password)
    # if email and password correspond to a valid user, return that user
    user = User.find_by(email: email)
    if user.password == password
      user
    else
      nil
    end
  end
end

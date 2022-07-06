class User < ActiveRecord::Base
  has_many :cards
  has_many :study_sessions
  has_many :tags
end

class Account < ActiveRecord::Base
  attr_accessible :email, :id, :password
end

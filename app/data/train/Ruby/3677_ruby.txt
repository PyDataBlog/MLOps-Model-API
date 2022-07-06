Rails.application.routes.draw do

  mount Abdesign::Engine => "/abdesign"

  get "/index" => "dummy#index"
end

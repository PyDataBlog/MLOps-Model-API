Rails.application.routes.draw do
  root 'dashboards#index'
  get 'dashboards/index'

  resources :petitions
  devise_for :users
  # For details on the DSL available within this file, see http://guides.rubyonrails.org/routing.html
end

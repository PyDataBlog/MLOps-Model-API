Rails.application.routes.draw do
  get 'accounts/show'
  patch 'accounts/update'
  post 'accounts/create'

  devise_for :users
  root 'application#index'

  resources :songs, only: [:show, :index, :create]
  resources :users, only: [:show]

end

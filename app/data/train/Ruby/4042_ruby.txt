Rails.application.routes.draw do
  root 'badge#new'

  get '' => 'badge#new'
  # create new badge-url for given parameters
  post '' => 'badge#create'

  # request badge
  get ':id.png' => 'badge#show', as: :badge

end

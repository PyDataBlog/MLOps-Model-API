module Web::Controllers::Home
  class Index
    include Web::Action

    def call(params)
      if UserRepository.logged_in? session
        redirect_to '/topics'
      end
    end
  end
end

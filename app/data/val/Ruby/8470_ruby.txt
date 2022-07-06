class ApplicationController < ActionController::Base
  protect_from_forgery

  helper_method :counter_session, :cats, :journalists

  

private

def counter_session
	@counter_session ||= session[:counter_id]
end 

def journalists

	@journalists = User.all.distinct(:username)
	
end

def cats
	@cats = Newscategory.all.distinct(:category) 	
end

end

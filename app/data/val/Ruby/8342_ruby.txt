get '/users/new' do
  erb :'users/new'
end

post '/users/new' do
  #if user already exists
  if User.find_by(email: params[:user][:email])
    erb :'users/new'
  else
    @user = User.create(params[:user])
      session[:user_id] = @user.id
      redirect "/users/#{@user.id}"
  end
end

get '/login' do
  erb :'users/login'
end

post '/login' do
  #if password matches database, go to profile
  @user = User.find_by(email: params[:user][:email])
  if @user && @user.password == params[:user][:password]
    session[:user_id] = @user.id
    redirect "/users/#{@user.id}"
  else
    erb :'users/login'
  end
end

get '/logout' do
  session.delete(:user_id)
  redirect '/'
end

get '/users/:id' do
  ensure_login
  erb :'users/show'
end

def ensure_login
  if session[:user_id].nil?
    redirect "/login"
  end
end

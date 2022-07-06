get '/user' do
  if logged_in?
    redirect '/'
  end
  erb :create_account
end

post '/user' do
  user = User.create(params[:user])
  redirect '/'
end

post '/login' do
  if user = User.login(params[:user])
    session[:permissions] = '3'
  end
  redirect '/'
end

get '/logout' do
  session.clear
  session[:permissions] = '0'
  redirect '/'
end

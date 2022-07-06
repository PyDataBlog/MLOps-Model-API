class UsersController < ApplicationController
  before_action :is_authenticated?, except: [:new, :create]
  # respond_to :json
      # layout 'splash', only: [:new, :create]

  def index

    respond_to do |format|
      format.html { redirect_to user_path(current_user.id) } 
      format.json { render json: User.all }
    end
  end

  def show
    current_user
    @collections = Collection.all
    # @user = User.find_by(id: params[:id])
    # @current_user = current_user.user
  end

  def new
    @user = User.new
  end


  def create
    @user = User.new( user_params )
    @user.save

    respond_to do |format|
      if @user.save
        format.html { redirect_to login_path, notice: 'User was successfully created.  Please Login.' }
        format.json { render action: 'show', status: :created, location: @user }
      else
        format.html { render action: 'new' }
        format.json { render json: @user.errors, status: :unprocessable_entity }
      end
    end
  end

  def edit
    # @user = User.find_by(id: params[:id])
    @user = current_user
  end

  def update
    @user = current_user
    # @item = Item.find_by(id: params[:id])
    if @user.update_attributes(user_params)
      redirect_to user_path(@user)
    else 
      render action: 'edit', status: :unprocessable_entity, location: @item
    end
  end

  def destroy
    @user = current_user
    @user.destroy
    redirect_to root_url
  end

  private

  def user_params
    params.require(:user).permit(:name, :last_name, :email, :password, :password_confirmation)
  end

end

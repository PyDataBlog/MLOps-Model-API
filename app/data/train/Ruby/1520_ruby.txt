class UsersController < ApplicationController
# before_action :set_user, only: [:guchi, :jiman, :inori, :index, :show, :edit, :update, :destroy]
before_action :logged_in_user, only: [:guchi, :jiman, :inori, :index, :show, :edit, :update, :destroy]
    
  def list
    @msg = 'Users cont. list アクション'
    @user = User.find(params[:id])
  end

  def guchi
    @msg = 'Users cont. guchi アクション'
    @user = User.find(params[:id])
    @micropost = Micropost.new
  end
  
  def guchi_feedback
    @msg = 'Users cont. guchi_feedback アクション'
  end
  
  def jiman
    @msg = 'Users cont. jiman アクション'
    @user = User.find(params[:id])
    @micropost = Micropost.new
  end
  
  def jiman_feedback
    @msg = 'Users cont. jiman_feedback アクション'
  end
  
  def inori
    @msg = 'Users cont. inori アクション'
    @user = User.find(params[:id])
    @micropost = Micropost.new
  end

  def inori_feedback
    @msg = 'Users cont. inori_feedback アクション'
  end
  
  def index
    @msg = 'Users cont. index アクション'
    @user = User.find(params[:id])
  end
  
  def show
    @msg = 'Users cont. show アクション'
    @user = User.find(params[:id])
    @microposts = @user.microposts.find(params[:id])
  end
  
  
  def edit
  @msg = 'Users cont. edit アクション'
  @user = User.find(params[:id])
  end

  def new
    @msg = 'Users cont. new アクション'
    @user = User.new
  end
  
  def create
    @user = User.new(user_params)
    if @user.save
       log_in @user
      flash[:success] = "Welcome to the SressShedder App!"
       redirect_to @user
      # Handle a successful save.
    else
      render 'new'
    end
  end

  private
  
    # Use callbacks to share common setup or constraints between actions.
    def set_user
      @user = User.find(params[:id])
      @micropost = Micropost.find(params[:id])
    end
    
    def user_params
      params.require(:user).permit(:name, :email, :password,
                                   :password_confirmation)
    end
    
     # beforeフィルター

    # 正しいユーザーかどうかを確認
    def correct_user
      @user = User.find(params[:id])
      redirect_to(root_url) unless current_user?(@user)
    end
end

class MessagesController < ApplicationController

  before_action :find_message, only: [:show, :edit, :update, :destroy]
  before_action :authenticate_user!, except: [:index, :show]

  def index
    @messages = Message.all.order("created_at DESC").page params[:page]
  end

  def show
    #@comment = current_user.messages.build.comments.build
    messages = Message.all.reverse
    id = params[:id]
    @page = 2
    messages.each_with_index do |m, i|
      if m.id == id.to_i
        @page = (i/9) + 1 
      end
    end

  end

  def new
    @message = current_user.messages.build
  end

  def create
    @message = current_user.messages.build(message_params)
    if @message.save
      redirect_to root_path
    else
      render 'new'
    end
  end

  def edit
  end

  def update
    if @message.update(message_params)
      redirect_to message_path
    else
      render 'edit'
    end
  end

  def destroy
    @message.destroy
    redirect_to root_path
  end

  def sample
    message = Message.all.sample
    data = {}
    data['message'] = message.content
    data['author'] = message.user.name
    data['href'] = "/messages/" + message.id.to_s
    render json: data

  end

  def presentation

  end


  private
    def message_params
      params.require(:message).permit(:content)
    end

    def find_message
      @message = Message.find(params[:id])
    end

end

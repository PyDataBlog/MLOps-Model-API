class StreamersController < ApplicationController
  before_action :set_streamer, only: [:show, :edit, :update, :destroy]
  layout 'gaming'

  def index
    @streamers = Streamer.all
  end

  def show
  end

  def new
    @streamer = Streamer.new
  end

  def edit
  end

  def create
    @streamer = Streamer.new
    # @streamer.user_id = current_user.id
    @streamer = Streamer.new(streamer_params)
    respond_to do |format|
      if @streamer.save
        format.html { redirect_to streamers_path, notice: 'Streamer was successfully created.' }
        format.json { render :index, status: :created, location: @streamer }
      else
        format.html { render :new }
        format.json { render json: @streamer.errors, status: :unprocessable_entity }
      end
    end
  end

  def update
    respond_to do |format|
      if @streamer.update(streamer_params)
        format.html { redirect_to @streamer, notice: 'Streamer was successfully updated.' }
        format.json { render :show, status: :ok, location: @streamer }
      else
        format.html { render :edit }
        format.json { render json: @streamer.errors, status: :unprocessable_entity }
      end
    end
  end


  def destroy
    @streamer.destroy
    respond_to do |format|
      format.html { redirect_to streamers_url, notice: 'Streamer was successfully destroyed.' }
      format.json { head :no_content }
    end
  end

  private
  def set_streamer
    @streamer = Streamer.find(params[:id])
  end

  def streamer_params
    params.require(:streamer).permit(:handle, :profile, :games, :avatar, :twitch, :youtube, :twitter, :instagram, :facebook, :user_id)
  end
end

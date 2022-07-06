class ConfidantsController < ApplicationController
  before_action :set_confidant, only: [:show, :edit, :update, :destroy]

  # GET /confidants
  # GET /confidants.json
  def index
    @confidants = Confidant.all
  end

  # GET /confidants/1
  # GET /confidants/1.json
  def show
  end

  # GET /confidants/new
  def new
    @confidant = Confidant.new
  end

  # GET /confidants/1/edit
  def edit
  end

  # POST /confidants
  # POST /confidants.json
  def create
    @confidant = Confidant.new(confidant_params)

    respond_to do |format|
      if @confidant.save
        format.html { redirect_to @confidant, notice: 'Confidant was successfully created.' }
        format.json { render :show, status: :created, location: @confidant }
      else
        format.html { render :new }
        format.json { render json: @confidant.errors, status: :unprocessable_entity }
      end
    end
  end

  # PATCH/PUT /confidants/1
  # PATCH/PUT /confidants/1.json
  def update
    respond_to do |format|
      if @confidant.update(confidant_params)
        format.html { redirect_to @confidant, notice: 'Confidant was successfully updated.' }
        format.json { render :show, status: :ok, location: @confidant }
      else
        format.html { render :edit }
        format.json { render json: @confidant.errors, status: :unprocessable_entity }
      end
    end
  end

  # DELETE /confidants/1
  # DELETE /confidants/1.json
  def destroy
    @confidant.destroy
    respond_to do |format|
      format.html { redirect_to confidants_url, notice: 'Confidant was successfully destroyed.' }
      format.json { head :no_content }
    end
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_confidant
      @confidant = Confidant.find(params[:id])
    end

    # Never trust parameters from the scary internet, only allow the white list through.
    def confidant_params
      params.require(:confidant).permit(:arcana, :name, :start_date, :notes)
    end
end

class DiscountTechnicalTypesController < ApplicationController
  before_action :set_discount_technical_type, only: [:show, :edit, :update, :destroy]

  # GET /discount_technical_types
  # GET /discount_technical_types.json
  def index
    @discount_technical_types = DiscountTechnicalType.all
  end

  # GET /discount_technical_types/1
  # GET /discount_technical_types/1.json
  def show
  end

  # GET /discount_technical_types/new
  def new
    @discount_technical_type = DiscountTechnicalType.new
  end

  # GET /discount_technical_types/1/edit
  def edit
  end

  # POST /discount_technical_types
  # POST /discount_technical_types.json
  def create
    @discount_technical_type = DiscountTechnicalType.new(discount_technical_type_params)

    respond_to do |format|
      if @discount_technical_type.save
        format.html { redirect_to @discount_technical_type, notice: 'Discount technical type was successfully created.' }
        format.json { render :show, status: :created, location: @discount_technical_type }
      else
        format.html { render :new }
        format.json { render json: @discount_technical_type.errors, status: :unprocessable_entity }
      end
    end
  end

  # PATCH/PUT /discount_technical_types/1
  # PATCH/PUT /discount_technical_types/1.json
  def update
    respond_to do |format|
      if @discount_technical_type.update(discount_technical_type_params)
        format.html { redirect_to @discount_technical_type, notice: 'Discount technical type was successfully updated.' }
        format.json { render :show, status: :ok, location: @discount_technical_type }
      else
        format.html { render :edit }
        format.json { render json: @discount_technical_type.errors, status: :unprocessable_entity }
      end
    end
  end

  # DELETE /discount_technical_types/1
  # DELETE /discount_technical_types/1.json
  def destroy
    @discount_technical_type.destroy
    respond_to do |format|
      format.html { redirect_to discount_technical_types_url, notice: 'Discount technical type was successfully destroyed.' }
      format.json { head :no_content }
    end
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_discount_technical_type
      @discount_technical_type = DiscountTechnicalType.find(params[:id])
    end

    # Never trust parameters from the scary internet, only allow the white list through.
    def discount_technical_type_params
      params.require(:discount_technical_type).permit(:value)
    end
end

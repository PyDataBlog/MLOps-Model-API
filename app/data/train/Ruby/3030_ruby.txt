class FoodItemController < ApplicationController
  def show
    # @id_requested = params[:id]
    @foodItemShow = FoodItem.where(id: params[:id]).first
  end
end

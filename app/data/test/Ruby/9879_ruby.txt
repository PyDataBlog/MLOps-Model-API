module KoolHints
  class HintsController < ApplicationController
    load_and_authorize_resource

    def edit
    end

    def update
      if @hint.update_attributes(params[:hint])
        flash[:notice] = "Hint updated"
        redirect_to return_to_path
      else
        render :edit
      end
    end
  end
end

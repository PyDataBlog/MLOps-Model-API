require_dependency "bcms_garment_district/application_controller"

module BcmsGarmentDistrict
  class PreviewsController < ApplicationController

    layout :if_not_xhr

    def create
      @markdown = MarkdownBlock.new params[:markdown_block]
    end

    private

    def if_not_xhr
      !request.xhr?
    end
  end
end
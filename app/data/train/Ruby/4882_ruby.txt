class LanguagesController < ApplicationController

	def index
	end

	def show
		@language = Language.find(params[:id])
    @query = @language.queries.new
    @queries = @language.queries.order(created_at: :desc)
    if params[:search]
      q = "%#{params[:search]}%"
      @queries = @queries.where("english like ?", q)
    end
  end

  def search
    @language = Language.find(params[:id])
    @queries = @language.queries
  end
end

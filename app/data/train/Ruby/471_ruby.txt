class HomeController < ApplicationController
  def index
    @movies = Movie.all.sort_by{|movie| movie.rank}.reverse.first(7)
    @books = Book.all.sort_by{|book| book.rank}.reverse.first(7)
    @albums = Album.all.sort_by{|album| album.rank}.reverse.first(7)
  end
end

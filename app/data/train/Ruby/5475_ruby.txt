require 'csv'
require 'pry'
require 'colorize'

require './player.rb'
require './board.rb'

module Game
  class Play
    def initialize
      @players = []
      @czar = ""
      get_players
      @board = Game::Board.new
      @players.each do |player|
        @board.deal_white_cards(player)
      end
    end

    def get_players
      puts "Let's play Cards Against Humanity!"
      puts "How many players do you have?"
      num_players = gets.chomp.to_i
      count = 0
      player_names = []
      while count < num_players
        puts "Please enter a player's name:"
        player_name = gets.chomp
        count += 1
        player_names.push(player_name)
      end
      player_names.each do |name|
        player = Player.new(name)
        @players.push(player)
      end
    end

    def assign_czar
      if @czar == ""
        @czar = @players.sample
      else
        old_czar_index = @players.index(@czar)
        new_czar_index = old_czar_index + 1
        if new_czar_index >= @players.length
          new_czar_index -= @players.length
        end
        @czar = @players[new_czar_index]
      end
    end

    def play
      puts "Now we are playing!"
      assign_czar
      puts "#{@czar.name.upcase} is the current Czar. Pass them the computer!"
    end


  end
end

a = Game::Play.new
a.play

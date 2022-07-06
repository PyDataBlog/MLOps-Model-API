#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

# 一个选随机数的程序，并告诉玩家是大还是小

class Game

  def initialize
    @answer = rand 10
    puts @answer
    ask
  end

  def ask
    puts "猜猜这个数（0-9）是什么？"
    unless test gets.to_i
      ask
    end
  end

  def test(guess)
    if guess === @answer
      puts "对了"
      true
    else
      if guess > @answer
        puts "大了"
      else
        puts "小了"
      end
      false
    end
  end

end

game = Game.new

#!/usr/bin/env ruby

require 'trello'
require 'colorize'

print "Welcome to Sprintify\n".green

if ENV["DEV_KEY"].nil? || ENV["MEMBER_KEY"].nil?
  print "Dev and Members keys are not present in environment\n".red
  print "Did you source the env file?\n".blue
  exit 1
end

print "Configuring Trello\n".yellow

Trello.configure do |config|
  config.developer_public_key = ENV["DEV_KEY"] # The "key" from step 1
  config.member_token = ENV["MEMBER_KEY"] # The token from step 3.
end

source_board = Trello::Board.all

print "Please Select Your Board\n".blue

source_board.each_with_index do |item, index|
  print ("  " + index.to_s + " " + item.name.to_s + "\n")
end

input = gets.chomp.to_i

if input.nil? || input < 0 || input > source_board.length
  print "Invalid Input\n".red
  exit 1
end

source_board = source_board[input]

print ("You have picked: " + source_board.name + "\n").blue

if source_board.closed?
  print "The Board is closed".red
  exit 1
end

source_list = source_board.lists

print "Please Select Your Source List\n".blue

source_list.each_with_index do |item, index|
  print ("  " + index.to_s + " " + item.name.to_s + "\n")
end

input = gets.chomp.to_i

if input.nil? || input < 0 || input > source_list.length
  print "Invalid Input\n".red
  exit 1
end

source_list_index = input

print "Please Select Your Destination List\n".blue

source_list.each_with_index do |item, index|
  print ("  " + index.to_s + " " + item.name.to_s + "\n")
end

input = gets.chomp.to_i

if input.nil? || input < 0 || input > source_list.length
  print "Invalid Input\n".red
  exit 1
end

dest_list_index = input
dest_list = source_list[dest_list_index]
source_list = source_list[source_list_index]


print ("Your source list is: " + source_list.name + "\n").blue
print ("Your destination list is: " + dest_list.name + "\n").blue

print "\n\nWould you like to proceed? (y/n)\n"
input = gets.chomp

if input.downcase == "n"
  print "Operation Aborted\n".red
  exit 0
end

if input.downcase != "y"
  print "Invalid Input\n".red
  exit 1
end

new_cards = Hash.new
current_title = ""

source_list.cards.each do |card|
  items = card.desc.split("\n").reject { |x| x == "" }
  
  items.each_with_index do |item, index|
    
    if items[index + 1] == "---"
      new_cards[item] = Array.new
      current_title = item
    elsif item == "---"
      next
    else
      new_cards[current_title] << item
    end
  end
end

check_list_name = "Acceptance Criteria"

new_cards.each do |key,values|
  current_card = Trello::Card.create(name: key, list_id: dest_list.id)
  current_checklist = Trello::Checklist.create(name: check_list_name, board_id: source_board.id)
  current_card.add_checklist(current_checklist)
  
  values.each do |item|
    item = item.gsub("-","")
    
    current_checklist.add_item(item)
  end
end

print "Operation Successfully Completed!".green




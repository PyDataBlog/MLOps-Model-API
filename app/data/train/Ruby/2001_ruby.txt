# A Nested Array to Model a Bingo Board SOLO CHALLENGE

# I spent [#] hours on this challenge.

#The first number in the sub-arrays correspond to the letter "B", the second "I", the third "N", the fourth "G", and the fifth "O". So if the call was N34, you would look in the third column for the number 34. If the number is on your board, you'll replace it with an "X."


# Release 0: Pseudocode
# Outline:

# Create a method to generate a letter ( b, i, n, g, o) and a number (1-100)      done

#bingo should correspond to the index of the number within the array... probably going to have to try an if loop here. if number @ index is called, return the letter that corresponds to that index. done

# Check the called column for the number called. done
#identify the index of the number called, index == column

# If the number is in the column, replace with an 'x' done
#if element is identified in that particular array, replace with x; else nothing. done

# Display a column to the console done
#column should display at the end of each number done

# Display the board to the console (prettily)
  #this will be a pain. it's done. It's not awesome, but I'm ok with that.

# Initial Solution
=begin
class BingoBoard
  attr_accessor :bingo_board
  attr_reader :letters
  attr_accessor :call_out

  def initialize(board)
    @bingo_board = board
    @letters = ["B", "I", "N", "G", "O"]
    @call_out = []
  end

#Create a method to call letters and numbers (like B43). It will need to generate a letter ( "B", "I", "N", "G", "O") and a number from 1 to 100.
  def call_out_update
    @call_out[0] = letters.sample
    @call_out[1] = rand 1..100
    p @call_out
  end



  def check_board(call_out)
    called_letter = call_out[0]
    called_num = call_out[1]
    check_letters = []
    check_letters << @bingo_board
#need to find the number in the array. if the number is in index 0, return B, if it's in index 1, return I, if it's in index 2 return N, else G, else 0
#index is an array method, so you can't call index on the element itself. this is also an enumerator.
    if called_letter == 'B'
      i = 0
      check_letters.map! { |array|
        if array.is_a? Array
          array.each { |array_num|
            if called_num == array_num[0]
              array[i].shift
              array[i].insert(0, 'x')
            else
              i += 1
            end
          }
        end
      }
    elsif called_letter == 'I'
      i = 0
      check_letters.map! { |array|
        if array.is_a? Array
          array.each { |array_num|
            if called_num == array_num[1]
              array[i].shift
              array[i].insert(1, 'x')
            else
              i += 1
            end
          }
        end
      }
    elsif called_letter == 'N'
          i = 0
      check_letters.map! { |array|
        if array.is_a? Array
          array.each { |array_num|
            if called_num == array_num[2]
              array[i].shift
              array[i].insert(2, 'x')
            else
              i += 1
            end
          }
        end

      }
    elsif called_letter == 'G'
          i = 0
      check_letters.map! { |array|
        if array.is_a? Array
          array.each { |array_num|
            if called_num == array_num[3]
              array[i].shift
              array[i].insert(3, 'x')
            else
              i += 1
            end
          }
        end

      }
    elsif called_letter == 'O'
          i = 0
      check_letters.map! { |array|
        if array.is_a? Array
          array.each { |array_num|
            if called_num == array_num[4]
              array[i].shift
              array[i].insert(4, 'x')
            else
              i += 1
            end
          }
        end
      }
    else
      puts "what?"
    end

    check_letters.flatten!(1)
    @bingo_board == check_letters
    puts @bingo_board[0].to_s
    puts @bingo_board[1].to_s
    puts @bingo_board[2].to_s
    puts @bingo_board[3].to_s
    puts @bingo_board[4].to_s
  end
end

=end
# Refactored Solution

# I spent over 3 hours on this. It needs to be refactored. I'm tomboxing myself, if there is time at the end of the week I will refactor. This is some seriously awful code work though, super repetetive. I will try to refactor it by the end of the week.


class BingoBoard
  attr_accessor :bingo_board
  attr_reader :letters
  attr_accessor :call_out

  def initialize(board)
    @bingo_board = board
    @letters = ["B", "I", "N", "G", "O"]
    @call_out = []
  end

#Create a method to call letters and numbers (like B43). It will need to generate a letter ( "B", "I", "N", "G", "O") and a number from 1 to 100.
  def call_out_update
    @call_out[0] = letters.sample
    @call_out[1] = rand 1..100
    p @call_out
  end

  def check_board(call_out)
    called_letter = call_out[0]
    called_num = call_out[1]
    check_letters = []
    check_letters << @bingo_board

  bingo_hash = {
    "B" => 0,
    "I" => 1,
    "N" => 2,
    "G" => 3,
    "O" => 4
  }

  if bingo_hash.has_key?(called_letter)
    check_letters.map! { |element|
      if element.is_a? Array
        element.each { |array_num|
          if called_num == array_num[bingo_hash[called_letter]]
            element[bingo_hash[called_letter]].shift
            element[bingo_hash[called_letter]].insert(bingo_hash[called_letter], 'x')
          end
        }
      end
    }
  else
    puts "what?"
  end

    check_letters.flatten!(1)
    @bingo_board == check_letters
    puts @bingo_board[0].to_s
    puts @bingo_board[1].to_s
    puts @bingo_board[2].to_s
    puts @bingo_board[3].to_s
    puts @bingo_board[4].to_s
  end
end


#DRIVER CODE (I.E. METHOD CALLS) GO BELOW THIS LINE
board = [[47, 44, 71, 8, 88],
        [22, 69, 75, 65, 73],
        [84, 85, 97, 89, 57],
        [25, 31, 96, 68, 51],
        [75, 70, 54, 80, 83]]

new_game = BingoBoard.new(board)
new_game.check_board(new_game.call_out_update)


#Reflection
=begin


    How difficult was pseudocoding this challenge? What do you think of your pseudocoding style?
I thought the pseudo coding was ok when I did it. When I went to code I realized I was focusing on the wrong thing and copied in the actual items to focus on. My pseudocoding style needs work as a whole.

    What are the benefits of using a class for this challenge?
a class can pass information between it's methods easier than methods not contained by a class can.

    How can you access coordinates in a nested array?
array[0][0] is an example. It pulls the first item from the first array within the array. you can stack as needed.

    What methods did you use to access and modify the array?
map * a lot. there are totally better ways to do this. I couldn't find them on my initial solution.

    Give an example of a new method you learned while reviewing the Ruby docs. Based on what you see in the docs, what purpose does it serve, and how is it called?
haven't reviewed any for this. I did use each_with_index on the 6.7 challenge. I may try to incorporate that here. it seems like it would cut down the mountains / valleys I created here.

    How did you determine what should be an instance variable versus a local variable?
By not planning ahead. I ended up trying to use as many instance variables as possible in the end so that way the information could be used in the entire class.

    What do you feel is most improved in your refactored solution?
      results pending on that one.
=end

#
# Copyright (c) 2013 Travis W. Peters
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not 
# use this file except in compliance with the License. You may obtain a copy 
# of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software 
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT 
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the 
# License for the specific language governing permissions and limitations under 
# the License.
#

require 'optparse'
require 'termchars'

class TermText

  include TermChars

  attr_accessor :out_string

  def initialize(out_string = "")
    # Text is composed of 7 distinct rows of characters
    @line_height = 7
    @lines       = ["","","","","","",""]
    @out_string  = out_string
  end

  def append(rows = [])
    raise ArgumentError unless rows.size == @line_height
    (0..@line_height-1).each{ |i|
      @lines[i] += rows[i]
    }
  end

  def clear!
    @out_string = ""
    @lines = ["","","","","","",""]
  end

  def construct(str = "")
    str.empty? ? str = @out_string : @out_string = str

    str.split("").each{ |char|
#    str.each_char{ |char| # broken/not suported in Ruby 1.8
      case char
      when "a".."z"
        append(send(char.to_sym))
      when "A".."Z"
        append(send(char.downcase.to_sym))
      when " "
        append(space)        
      else
          puts "I'm sorry but the '#{char}' character is not yet defined."
      end
    }
    @lines
  end

  def display(reset = true)
    check_empty
    puts @lines
    reset ? clear! : nil
  end

  def check_empty
    @out_string.empty? ? construct("empty string") : nil    
  end

end #TermText

def options 
  OptionParser.new do |o|
    o.on('-i') { |b| $interactive = b }
    o.on('-h') { puts o; exit }
    o.parse!
  end
end

def run_tests
  # TEST initialization & implemented characters
  text = TermText.new
  puts "Display the alphabet:"
  text.out_string = "abcdefghijklmnopqrstuvwxyz"
  text.construct
  text.display

  # TEST unimplemented characters
  puts "Display unimplemented characters:"
  text.construct("~ ! @ # $ % ^ & * ( ) - _ = + [ ] { } \\ | / . , < >")
  text.display

  #TODO: if @lines is empty or only spaces, the line should display "empty string"

  # TEST empty string
  puts "Display the empty string text:"
  text.construct("")
  text.display

  # TEST 'clear!' method where the default is to "clear" the out_string
  text.construct("LOCA")
  text.display
  text.construct("Peters")
  text.display(false)
  text.construct("abcd")
  text.display(true)
end

def run_input
  text = TermText.new
  text.out_string = ARGV.join(" ")
  text.construct
  text.display
end

# MAIN
if __FILE__ == $PROGRAM_NAME

  options

  ARGV.size > 0 ? run_input : run_tests

end

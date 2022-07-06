# gets the number of voters and the number of candidates and makes the tables
def set_up
	puts "How many candidates are there?"
	$cands = gets.chomp.to_i
	$magic_num = (($cands / 2).ceil + 1)
	puts "The magic number is: #{$magic_num}"
	$cand_table = Array.new($cands) { Array.new(2, 0) }

	# This table holds the actual votes, each top array is a voter and
	# each lower array holds their votes in ranked order
	puts "How many voters are there?"
	$voters = gets.chomp.to_i
	$votes_table = Array.new($voters) { Array.new($cands) }
	$stop = false
end

def populate_random
	$cands = 5
	$voters = 17
	$candidates = ["Jeremy", "Andrew", "Mark", "Paula", "Bret"]
	$votes_table = Array.new($voters) {$candidates.shuffle}
	$magic_num = (($cands / 2).ceil + 1)
	$stop = false
	$cand_table = [["Jeremy", 0], ["Andrew", 0], ["Mark", 0], ["Paula", 0], ["Bret", 0]]
end

# gets the names of all of the candidates and stores it as a string in the table
def get_candidates
	$cand_table.each do |x|
		puts "Give me a candidate name:"
		x[0] = gets.chomp
	end
end

# gets all the voters' votes and stores them in the table
def get_votes
	$votes_table.each do |voter|
		voter.each do |vote|
			puts "Give me your vote"
			vote = gets.chomp
		end
	end
end

class counting

	@votes = $votes_table
	@names = $cand_table

	def tops_first
		puts "Setting tops"
		@tops = Array.new
		$votes_table.each do |x|
			@tops.push(x[0])
		end
	end

	def tops_again
		puts "Setting tops again"
		@tops = Array.new
		@votes.each do |x|
			@tops.push(x[0])
		end
	end

	def count_up
		puts "Counting..."
		@names.each do |x|
			x[1] = @tops.count(x[0])
		end
		count_check
	end

	def count_check
		puts "Checking the count..."
		@names.each do |x|
			if x[1] >= $magic_num
				$stop = true
				winner(x[0])
			end
		end
		if $stop == false
			puts "Removing candidate(s)..."
			set_bottom
		end
	end


	def set_bottom

		# puts the votes in a new array
		nums = Array.new
		@names.each do |x|
			nums.push(x[1])
		end

		@bottom = Array.new

		# sets the candidate with the same index as the lowest number to bottom
		@names.each do |x|
			if x[1] == nums.min
				@bottom.push(x[0])
			end
		end

		delete_bottom
	end

	def delete_bottom
		# deletes bottom from the votes
		@bottom.each do |y|
      @votes.each do |x|
        x.delete_if {|e| e == y}
      end
		end

		# deletes bottom from the names
		@bottom.each do |x|
			@names.delete_if { |e| e.include?(x) }
		end
		puts "Removing #{@bottom}"

		if @bottom.empty?
      puts "No one elected."
    else
      tops_again
      count_up
    end
	end

	def winner(name)
		puts "#{name} is elected!"
	end
end

set_up
get_candidates
get_votes
tops_first
count_up

#silly_blocks.rb

def reverser
	array = yield.split(' ')
	array.map! {|word| word.reverse}
	array.join(' ')
end

def adder(a = 1)
	yield + a
end

def repeater(n = 1)
	n.times do
		yield		
	end
end

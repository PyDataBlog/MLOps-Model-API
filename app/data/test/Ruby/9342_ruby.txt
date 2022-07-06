# Calculate a Grade

# I worked on this challenge with: Jack Thatcher.

# Your Solution Below

def get_grade (average)

	if average > 89
		then letter_grade = 'A'
	elsif average > 79
		then letter_grade = 'B'
	elsif average > 69
		then letter_grade = 'C'
	elsif average > 59
		then letter_grade = 'D'
	else
		letter_grade = 'F'
	end		

	return letter_grade

end


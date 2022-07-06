
student_phoneNumber_name = {1: 'a', 3: 'c', 2: 'b'}

def Handler() :
	while (1) :
		choice = eval(input("Enter :\t 1 - to search student name \n \t 2 - to insert new student record \n \t 0 - to quit\n"))
		print(choice)
		if (choice == 1) :
			if (student_phoneNumber_name) :
				phone_number = input("Enter student's phone number : ")
				name = SearchRecord(phone_number)
				if (name) :
					print("name : " + name )
				else :
					print(str(phone_number) + "Does not exist in record" + str(name))
			else :
				print("Record is empty ")
		elif (choice == 2) :
			phone_number = input("Enter student's phone number : ")
			name = input("Enter student's name : ") #best example to understand input() and raw_input()
			InsertRecord(phone_number, name)
		elif (choice == 0) :
			break
		else:
			print("Enter correct choice")
		

def InsertRecord(x, y):
	student_phoneNumber_name[x] = y
	return;
	
def SearchRecord(x):
	print(x)
	if (x in student_phoneNumber_name) :
		return student_phoneNumber_name[x]
	
	return False
	
		
Handler()

print(student_phoneNumber_name)
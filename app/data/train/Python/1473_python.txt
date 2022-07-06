#David Hickox
#Feb 15 17
#Squares (counter update)
#descriptions, description, much descripticve
#variables
#   limit, where it stops
#   num, sentry variable


#creates array if needbe
#array = [[0 for x in range(h)] for y in range(w)] 
#imports date time and curency handeling because i hate string formating (this takes the place of #$%.2f"%) 
import locale
locale.setlocale( locale.LC_ALL, '' )
#use locale.currency() for currency formating
print("Welcome to the (insert name here bumbblefack) Program\n")

limit = float(input("How many squares do you want? "))
num = 1
print("number\tnumber^2")
while num <= limit:
    #prints the number and squares it then seperates by a tab
    print (num,num**2,sep='\t')
    #increments
    num += 1




input("\nPress Enter to Exit")

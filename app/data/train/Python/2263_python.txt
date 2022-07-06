# Brandon Michael
# cis142
# checkForQuadrant.py
# Goal: This program will keep asking for input values to check for the quadrant postion,
# origin, x-axis and y axis postions
# Notes: I used a while loop to make testing values easier and I used the input x,y


# Display program instructions
print("###################################################")
print("Quadrant Finder 1.0")
print("Enter the x and y coordinates to find the quadrant!")
print("Type [exit] to quit the program")
print("###################################################")

# Setup the x and y variables
xValue = None
yValue = None

# Setup a loop that breaks when you type exit
while True:
    # Get the input values in a X,Y format
    inputCoordinates = input("Type in coordinates [x,y]: ")
    # Check if exit was typed, if so then exit the loop and end
    if inputCoordinates == "exit":
        break  # stops the loop
    # We want to make sure we can only strip out 2 input values
    # and make sure there is a comma separating them
    elif len(inputCoordinates.strip().split(',')) == 2 and inputCoordinates.count(',') == 1:
        # Loop over the two numbers that are stripped out by the comma value
        for coordinate in inputCoordinates.strip().split(','):
            # This checks to see if we have set a value for x
            # If it is still set to None then the first value is going to be xValue
            if xValue is None:
                xValue = int(coordinate)
            # Since we are checking the xValue we can assume when the loop comes back
            # a second time we can set it to yValue
            else:
                yValue = int(coordinate)
        # If its a 0,0 value then its the Origin
        if xValue == 0 and yValue == 0:
            print("Origin")
        else:
            # If x = 0 and the y is greater or less than 0 its on the Y axis
            if xValue == 0 and (yValue < 0 or yValue > 0):
                print("Y - Axis")
            # If x is greater or less than 0 and y = 0 its on the X axis
            elif (xValue < 0 or xValue > 0) and yValue == 0:
                print("X - Axis")
            # Anything else and we need to check for quadrants
            else:
                # If x is a positive number and y is a negative positive its in Quadrant 1
                if xValue > 0 and yValue > 0:
                    print("Quadrant I")
                # If x is a negative number and y is a positive number then its in Quadrant 2
                elif xValue < 0 and yValue > 0:
                    print("Quadrant II")
                # If x is a negative number and y is negative number then its in Quadrant 3
                elif xValue < 0 and yValue < 0:
                    print("Quadrant III")
                # If x is a positive number and y is a negative number then its in Quadrant 4
                elif xValue > 0 and yValue < 0:
                    print("Quadrant IV")
    # If they typed anything but 2 numbers separated by a comma then ask for the input again
    else:
        print("Please type the input value as x,y")
        print("Example: 1,-9")

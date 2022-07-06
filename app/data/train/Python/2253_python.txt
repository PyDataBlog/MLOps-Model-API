from fruits import validate_fruit

fruits = ["banana", "lemon", "apple", "orange", "batman"]

print fruits


def list_fruits(fruits, byName=True):

    if byName:
        # WARNING: this won't make a copy of the list and return it. It will change the list FOREVER
        fruits.sort()

    for index, fruit in enumerate(fruits):
        if validate_fruit(fruit):
            print "Fruit nr %d is %s" % (index, fruit)
        else:
            print "This %s is no fruit!" % (fruit)

list_fruits(fruits)

print fruits

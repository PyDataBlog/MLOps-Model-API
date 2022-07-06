import math
def area(a, b, c):
	A1 = ((4*math.pi)*a**2)
	A2 = ((4*math.pi)*b**2)
	A3 = ((4*math.pi)*c**2)
	Avg = (A1+A2+A3)/3
	return Avg

def output(a, b ,c , d, e):
	return """
Hello there, {}!
equation: ((4*math.pi)*{}**2)((4*math.pi)*{}**2)((4*math.pi)*{}**2)
Calculating average area of three spheres...

the answer is: {} 
""".format(a, b, c, d, e)

def main():
	Name = raw_input("Name: ")
	Area1 = raw_input("Area of 1st sphere: ")
	Area2 = raw_input("Area of 2nd sphere: ")
	Area3 = raw_input("Area of 3rd sphere: ")
	e = area(int(Area1), int(Area2), int(Area3))
	print output(Name, Area1, Area2, Area3, e)
main()



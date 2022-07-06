alpha = "abcdefghijklmnopqrstuvwxyz"

for n in range(0, 26, 1):
	print alpha[0:n+1]

for n in range(26, 1, -1):
	print alpha[0:n-1]

"""
alpha = "a"

m = ord(alpha)

n = 0

while n < m:
	print chr(m + 1) in range(65, 122)
	m += 1

for i in range(ord('a'), 123, 1):	
	print chr(i[0:m+1])

while m < 123:
	print chr(m[0:])


"""	
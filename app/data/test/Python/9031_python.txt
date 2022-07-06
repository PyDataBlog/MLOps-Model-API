def no_extreme(listed):
	"""
	Takes a list and chops off extreme ends
	"""
	del listed[0]
	del listed[-1:]
	return listed

def better_no_extreme(listed):
	"""
	why better ? For starters , does not modify original list
	"""
	return listed[1:-1]

t = ['a','b','c']
print t
print '\n'

print 'pop any element : by t.pop(1) or t.remove(\'b\') or del t[1]'
del t[1]
print t

st = ['a','b','c','d','e','f']
print st
del st[1:3]
print 'del t[1:3] works as well : ', st

print 'Mid part is : ',no_extreme(st)

str = raw_input("\nEnter a string to be converted to list : ")
listr = list(str)
print listr

str2=raw_input("\nEnter a line to be separated into words : ")
listr2 = str2.split()#separated at spaces
print listr2

print 'You can split a line into words by changing the parameter as str2.split(parameter)'
print 'this splits at - '

print 'joining statement : '
delimeter=' '
print delimeter.join(listr2)

print '\nNote: 2 identical lists are 2 objects ,so l_a is l_b for identical lists still says False'
print 'This does not happen for strings etc'
print 'l_a is l_b only true if assigned as l_b = l_a'

print '\n t.append(x) returns None , whereas t+[y] is not None'
print '\n Never t = t[1:] as empty , same goes for t.sort()'


print '\nDO\'s : t.append(x)\n t = t+[x] \n '
print 'Keep copy of original just in case : orig = t[:] \nt.sort()'
print '\nDONT\'s :  t.append([x])\n t = t.append(x) \n t + [x] \n t = t + x' 
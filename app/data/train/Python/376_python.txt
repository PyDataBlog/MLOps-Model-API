#!/usr/bin/python
"""
Since functions are function instances you can wrap them

Allow you to
- modify arguments
- modify function
- modify results
"""
call_count = 0
def count(func):
    def wrapper(*args, **kw):
        global call_count
        call_count += 1
        return func(*args, **kw)
    return wrapper

def hello():
	print 'Invoked hello'

hello = count(hello)  ## Now decorating hello to increment call count

hello()
print call_count
hello()
print call_count	    

"""
## Syntactic Sugar

>>> @count
... def hello():
...     print "Invoked hello"

equals

hello = count(hello)


## Syntactic Sugar 2
Dont add parens to the decorator
>>> @count()
... def hello():
...     print "Invoked hello"
... 
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: count() takes exactly 1 argument (0 given)
>>> 

##Decorator Template
def decorator(func_to_decorate):
	def wrapper(*args, **kwargs):
		# do something before invocation
		result = func_to_decorate(*args,**kwargs)
		# do something after invocation
		return result
	#update wrapper.__doc__ and .func_name
	# or functools.wraps	
	return wrapper	

##Decorators can also be classes, to have a class that Decorates
class decorator(object):
	def __init__(self, function):
		self.function = function
	def __call__(self, *args, **kw):
		# do something before invocation
		result = self.function(*args, **kw)
		# do something after
		return result	

##Decorators can also be classes 2, to have a instance that Decorates
class decorator(object):
	def __init__(self, function):
		self.function = function
	def __call__(self, *args, **kw):
		def wrapper(*args, **kw):
			# do something before invocation
			result = self.function(*args, **kw)
			# do something after
			return result	
		return wrapper

## The aboves lets you have an instance of a decorator that stores state
(rather than using global state)	

## Parameterized decorators (need 2 closures)
def limit(length):
	def decorator(function):
		def wrapper(*args, **kw):
			result = function(*args, **kw)
			result = result[:length]
			return result
		return wrapper
	return decorator			

>>> @limit(5)  ## Decorating the simple function echo with limit 5 as parameter
... def echo(foo):
...     return foo
... 
>>> echo ('123456')
'12345'
>>> 
Or you can use following as well , to limit the echo function with 3 as parameter
>>> echo = limit(3)(echo)
>>> echo ('123456')
'123'
>>> 

## Decorator Tidying

function attributes get mangled
>>> def echo2(input):
...     ###return input###  I used ### instead of 3  coz that was causing some error
...     return input
... 
>>> echo2.__doc__
'return input'
>>> echo2.func_name
'echo2'
>>> 
>>> echo3 = limit(3)(echo2)
>>> echo3.__doc__
>>> echo3.func_name
'wrapper'
>>> 

#Now to fix above define your limit decorator as below
def limit(length):
	def decorator(function):
		def wrapper(*args, **kw):
			result = function(*args, **kw)
			result = result[:length]
			return result
		wrapper.__doc__ = function.__doc__
		wrapper.func_name = function.func_name	
		return wrapper
	return decorator

>>> echo4 = limit(3)(echo2)
>>> echo4.__doc__
'return input'
>>> echo4.func_name
'echo2'
>>> 

#Decorator tidying (3) , using functools , more simple
import functools
def limit(length):
	def decorator(function):
		@functools.wraps(function)
		def wrapper(*args, **kw):
			result = function(*args, **kw)
			result = result[:length]
			return result
		#wrapper.__doc__ = function.__doc__
		#wrapper.func_name = function.func_name	
		return wrapper
	return decorator


Uses for decorator

- caching
- monkey patching stdio
- memoize
- jsonify
- logging time in function call
- change cwd
"""

def cwd_decorator(func):
	"""
	decorator to change cwd to directory containing rst for this function
	"""
	def wrapper(*args, **kw):
		cur_dir = os.getcwd()
		found = False
		for arg in sys.argv:
			if arg.endswith(".rst"):
				found = arg
				break
		if found:
			directory = os.path.dirname(arg)
			if directory:
				os.chdir(directory)
		data = func(*args, **kw)
		os.chdir(cur_dir)
		return data
	return wrapper					

"""
###
Properties
Call get/set methods via an instance attributes
class C(object):
	def getx(self):
		return self._x
	def setx(self, value):
		self._x = value
	def delx(self):
		del self._x
	x = property(getx, setx, delx, "I'm the 'x' property.")
	
from property.__doc__

"""				
import os
def find_files(base_dir, recurse=True):
	"""
	yeild files found in base_dir
	"""
	for name in os.listdir(base_dir):
		filepath = os.path.join(base_dir, name)
		if os.path.isdir(filepath) and recurse:
			for child in find_files(filepath, recurse):
				yield child
		else:
			yield filepath		



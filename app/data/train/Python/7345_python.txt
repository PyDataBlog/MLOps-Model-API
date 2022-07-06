import numpy


arr = numpy.array(list(map(float, input().split())))
x = float(input())
value = numpy.polyval(arr, x)
print(value)

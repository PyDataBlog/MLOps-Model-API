# Experiment with boundary for collections
L = [0, 1, 2, 3]
print('-------- Part A --------')
# Index beyond, generates a IndexError exception
try:
    L[4]  # part (a) of question
except IndexError as err:
    print('IndexError Exception', err)

print('-------- Part B --------')
# Slice out of bounds
sliced = L[-10:10]
print(sliced)
print('slicing out of bounds results in a new list equal in value to original')
print('if slices includes indices of original)')

print('-------- Part C --------')
# part(c), reverse slicing
sliced = L[3:1]
print(sliced)
# this is not same effect as out of bound slicing,
# results in a empty list above as start is greater than the end with a positive stride
# to actually reserve the values in a new list need to specify a negative stride
sliced = L[3:1:-1]
print(sliced)
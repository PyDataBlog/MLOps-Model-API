# Fluent Python Book
# List comprehensions are faster than for-loops

import time
from random import choices

symbols = list('abcdefghijklmn')
print(symbols)

symbols_big = choices(symbols, k=2000000)
# print(symbols_big)

start = time.time()
ord_list1 = []
for sym in symbols_big:
    ord_list1.append(ord(sym))
# print('ord list1:', ord_list1)
end = time.time()
print('for loop ran in %f s' % (end - start))

start = time.time()
# list comprehension
ord_list2 = [ord(sym) for sym in symbols_big]
# print('ord list2:', ord_list2)
end = time.time()
print('for loop ran in %f s' % (end - start))

# let's do a performance benchmark of this list comprehension
l_nums = [i for i in range(1000000)]

start = time.time()
sq_nums = []
for i in l_nums:
    sq_nums.append(i ** 2)
end = time.time()

print('for loop ran in %f s' % (end - start))

start = time.time()
sq_nums = [i ** 2 for i in range(1000000)]
end = time.time()

print('list comp ran in %f s' % (end - start))

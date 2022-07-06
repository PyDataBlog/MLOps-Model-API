
from __future__ import division, print_function #, unicode_literals

"""
Multiples of 3 and 5

If we list all the natural numbers below 10 that are multiples
of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
"""

import numpy as np

# Setup.

num_max = 1000

basis = [3, 5]

factors = []

for i in range(num_max):
    for k in basis:
        if not i % k:
            factors.append(i)
            break

print('\nRange: {:d}'.format(num_max))
print('Number of factors: {:d}'.format(len(factors)))
print('The answer: {:d}'.format(np.sum(factors)))

# Done.

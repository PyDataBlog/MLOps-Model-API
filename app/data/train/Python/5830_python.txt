"""
Hannah Aizenman
10/13/2013

Generates a random subset of size 10^P for p in [1,MAX_P) from [0, 10^8)
"""

import random

MAX_P = 8
max_value = 10**MAX_P
large_set = range(max_value)

for p in xrange(1,MAX_P):
    print "list of size: 10^{0}".format(p)
    f = open("p{0}.txt".format(p), 'w')
    sample = random.sample(large_set, 10**p)
    f.write("\n".join(map(lambda x: str(x), sample)))
    f.close()
    

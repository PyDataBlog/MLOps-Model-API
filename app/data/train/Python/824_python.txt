#!/bin/env python

import itertools
import collections


def read_table(filename):
    with open(filename) as fp:
        header = next(fp).split()
        rows = [line.split()[1:] for line in fp if line.strip()]
        columns = zip(*rows)
    data = dict(zip(header, columns))
    return data

table = read_table("../../data/colldata.txt")
pots = sorted(table)

alphabet = "+-?"
for num in range(2, len(table) + 1):
    for group in itertools.combinations(pots, num):
        patterns = zip(*[table[p] for p in group])
        counts = collections.Counter(patterns)
        for poss in itertools.product(alphabet, repeat=num):
            print ', '.join(group) + ':',
            print ''.join(poss), counts[poss]

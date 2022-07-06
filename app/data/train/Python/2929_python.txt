import sys

[_, ms, _, ns] = list(sys.stdin)
ms = set(int(m) for m in ms.split(' '))
ns = set(int(n) for n in ns.split(' '))
print(sep='\n', *sorted(ms.difference(ns).union(ns.difference(ms))))

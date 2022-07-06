#Python 2.7

candidates = set([str(a * b) for a in range(100, 1000) for b in range(100, 1000)])
candidates = filter(lambda x: x == x[::-1], candidates)
print max([int(x) for x in candidates])

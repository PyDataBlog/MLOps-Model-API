#!/usr/bin/env python
# https://www.codeeval.com/open_challenges/1/

import sys


def solve(X, Y, N):
    r = []
    for i in range(1, N + 1):
        if i % X == 0 and i % Y == 0:
            r.append('FB')
        elif i % X == 0:
            r.append('F')
        elif i % Y == 0:
            r.append('B')
        else:
            r.append(str(i))
    print ' '.join(r)


def main():
    for line in sys.stdin:
        (X, Y, N) = line.strip().split(' ')
        solve(int(X), int(Y), int(N))

if __name__ == '__main__':
    main()

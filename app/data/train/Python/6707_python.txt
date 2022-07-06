#!/usr/bin/env python
# encoding: utf-8

class MyRange(object):
    def __init__(self, n):
        self.idx = 0
        self.n = n
    def __iter__(self):
        return self
    def next(self):
        if self.idx < self.n:
            val = self.idx
            self.idx += 1
            return val
        else:
            raise StopIteration()

myRange = MyRange(3)
for i in myRange:
    print i


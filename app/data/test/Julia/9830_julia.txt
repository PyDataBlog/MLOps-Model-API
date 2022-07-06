#!/usr/bin/env julia
"""
input data:
3 5

answer:
8
"""
# woot, my first julia script :D
f=open(ARGS[1])
a=split(readline(f))

println(int(a[1])+int(a[2]))

T = int(raw_input())

for test in xrange(T):

    N = int(raw_input())

    a, b, result = 0, 1, 0

    c = a+b

    while c < N:

        if c%2 == 0:
            result += c
        
        a,b = b,c
        c = a+b

    print result
--Basically a brute force attempt. For larger numbers, say where a+b+c=10000, this will take a long long time. At 1000 though, it takes a minute or so
triplet n = [(x,y,z) | x <- [1..(n-1)], y <- [1..(n-x)], z <- [1..(n-x-y)], x+y+z == n, z>y, z>x, y>x, x^2+y^2==z^2]

problem9 = triplet 1000

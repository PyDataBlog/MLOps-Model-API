add (x,y) = x + y
add' x y = x + y

second xs = head (tail xs)

swap (x, y) = (y, x)

pair x y = (x, y) -- a -> b -> (a, b)

double x = x * 2 -- Num a => a -> a

palindrome xs = reverse xs == xs

-- How is the type of this function determined?
twice f x = f (f x) -- (a -> a) -> a -> a

f xs = take 3 (reverse xs)

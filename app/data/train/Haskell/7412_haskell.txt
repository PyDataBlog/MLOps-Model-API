-- Smallest multiple
-- Problem 5
-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

main = do
    print (getSmallestN)

getSmallestN =
    head [x | x <- [2520,2540..], all (isDivisibleBy x) [11..19]]

isDivisibleBy x y =
    x `mod` y == 0

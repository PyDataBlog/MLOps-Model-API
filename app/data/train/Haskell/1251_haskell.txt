module UCeuler3
( solve
) where

import Primes

-- https://en.wikipedia.org/wiki/Prime_factor#Perfect_squares
-- https://en.wikipedia.org/wiki/Square_number
--
-- fromIntegral: 
-- For example, given an Int value n, one does not simply take 
-- its square root by typing sqrt n, since sqrt can only be applied 
-- to Floating-point numbers. Instead, one must write 
-- sqrt (fromIntegral n) to explicitly convert n to a floating-point number.
--
-- '.' 
-- In Haskell, '.' works like the UNIX |, but "backwards".
perfect_squares n = takeWhile (< i) primes
  where i = round . sqrt . fromIntegral $ n

-- Anonymus function that filter if the reminder
-- for n divided by n is zero, i.e. evenly divided by perfect_squares input
-- (Perfect square numbers)
prime_factors n = filter (\x -> ((n `mod` x) == 0)) (perfect_squares n)

largest_prime_factor :: Integer -> Integer
largest_prime_factor n = maximum (prime_factors n)

solve :: Int -> [Char]
solve ucid = "Solved UC "++show(ucid)++": Result is: "++show(largest_prime_factor 600851475143)
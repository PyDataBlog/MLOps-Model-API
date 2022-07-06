-- Copyright (c) 2012, Christoph Pohl
-- BSD License (see http://www.opensource.org/licenses/BSD-3-Clause)
-------------------------------------------------------------------------------
--
-- Project Euler Problem 6
--
-- The sum of the squares of the first ten natural numbers is,
-- 1² + 2² + ... + 10² = 385
--
-- The square of the sum of the first ten natural numbers is,
-- (1 + 2 + ... + 10)² = 55² = 3025
--
-- Hence the difference between the sum of the squares of the first ten natural
-- numbers and the square of the sum is 3025 − 385 = 2640.
--
-- Find the difference between the sum of the squares of the first one hundred
-- natural numbers and the square of the sum.


module Main where
 
main :: IO ()
main = print result

result = squareOfSum - sumOfSquares

squareOfSum = (sum [1..100])^2

sumOfSquares = sum (map (^2) [1..100])


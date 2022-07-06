-- Copyright (c) 2012-2013, Christoph Pohl BSD License (see
-- http://www.opensource.org/licenses/BSD-3-Clause)
-------------------------------------------------------------------------------
--
-- Project Euler Problem 13
--
-- The following iterative sequence is defined for the set of positive
-- integers:
-- 
-- n → n/2 (n is even) n → 3n + 1 (n is odd)
-- 
-- Using the rule above and starting with 13, we generate the following
-- sequence: 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
-- 
-- It can be seen that this sequence (starting at 13 and finishing at 1)
-- contains 10 terms. Although it has not been proved yet (Collatz Problem), it
-- is thought that all starting numbers finish at 1.
-- 
-- Which starting number, under one million, produces the longest chain?
-- 
-- NOTE: Once the chain starts the terms are allowed to go above one million.

module Main where

import Data.List
import Data.Maybe

main :: IO ()
main = print result

result = 1 + fromMaybe 0 (elemIndex (maximum(listOfLengths)) listOfLengths)

listOfLengths :: [Int]
listOfLengths = map collatzLength [1..1000000]

collatzLength :: Integer -> Int
collatzLength n = collatzLength' n 1

collatzLength' :: Integer -> Int -> Int
collatzLength' n acc | n == 1 = acc
                     | even n = collatzLength' (n `div` 2) (acc+1)
                     | otherwise = collatzLength' (3*n + 1) (acc+1)


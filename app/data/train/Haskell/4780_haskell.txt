module Numbering
( dividable
, is_prime
) where

dividable :: Int -> Int -> Bool
dividable x y = (x `mod` y) == 0

is_prime :: Int -> Bool
is_prime 1 = False
is_prime x = length dividers == 0
    where dividers = (filter (dividable x) [2..floor (sqrt (fromIntegral x))])


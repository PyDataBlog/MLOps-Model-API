{- |
Module      : $Header$
Description : Various utility functions
License     : PublicDomain

-}
module Utils where

import Data.Digits(digits)
import Data.List(maximumBy)
import Data.Ord(comparing)

digitSum :: Integral n => n -> n
digitSum = sum . digits 10

digitLength :: Integral n => n -> Int
digitLength = length . digits 10

tripletSum :: Integral int => (int,int,int) -> int
tripletSum (a,b,c) = a+b+c

tripletProduct :: Integral int => (int,int,int) -> int
tripletProduct (a,b,c) = a*b*c

isPalindrome :: Show a => a -> Bool
isPalindrome a = (show a) == reverse (show a)

maxIndex :: Ord a => [a] -> Int
maxIndex = fst . maximumBy (comparing snd) . zip [0..]

isPythagoran :: Integral int => (int,int,int) -> Bool
isPythagoran (a,b,c) = a^2 + b^2 == c^2


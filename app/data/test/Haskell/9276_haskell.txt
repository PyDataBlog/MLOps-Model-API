module Problem8 ( removeDuplicates ) where

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates (x:xs) = if x == head xs then removeDuplicates xs else x : removeDuplicates xs
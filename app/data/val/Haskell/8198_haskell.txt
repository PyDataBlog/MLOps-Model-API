module DropNth where

--(Problem 16) Drop every N'th element from a list. 

dropNth :: [a] -> Int -> [a]  
dropNth [] _ = []
dropNth y@(x:xs) n = (take (n - 1) y) ++ dropNth (drop 2 xs) n

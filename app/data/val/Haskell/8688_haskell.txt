module Wholemeal
where

  fun1 :: [Integer] -> Integer
  fun1 = product . map (\x -> x - 2) . filter even

  fun2 :: Integer -> Integer
  fun2 1 = 0
  fun2 n
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

  fun3 :: Integer -> Integer
  fun3 = sum . filter even . takeWhile (>1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

  data Tree a = Leaf
              | Node Integer (Tree a) a (Tree a)

  --foldTree :: [a] -> Tree a

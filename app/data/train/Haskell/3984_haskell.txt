module Test where

import Prelude ()
import Zeno

data Nat = Zero | Succ Nat

length :: [a] -> Nat
length [] = Zero
length (x:xs) = Succ (length xs)

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

class Num a where
  (+) :: a -> a -> a

instance Num Nat where
  Zero + y = y
  Succ x + y = Succ (x + y)

class Eq a where
  (==) :: a -> a -> Bool

instance Eq Nat where
  Zero == Zero = True
  Succ x == Succ y = x == y
  _ == _ = False

prop_eq_ref :: Nat -> Prop
prop_eq_ref x = proveBool (x == x)

prop_length :: [a] -> [a] -> Prop
prop_length xs ys
  = prove (length (xs ++ ys) :=: length xs + length ys)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem n (x:xs)
  | n == x = True
  | otherwise = elem n xs

prop_elem :: Nat -> [Nat] -> [Nat] -> Prop
prop_elem n xs ys
  = givenBool (n `elem` ys)
  $ proveBool (n `elem` (xs ++ ys))

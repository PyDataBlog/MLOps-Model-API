module Polymorphism where

data Identity a = 
    Identity a

instance Eq a => Eq (Identity a) where
    (==) (Identity a1) (Identity a2) = a1 == a2

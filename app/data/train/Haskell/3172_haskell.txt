module GivenTypeWriteFunction where

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC (aToB a)

a :: (a -> c) -> a -> a
a aToC a = a

a' :: (a -> b) -> a -> b
a' aToB a = aToB a

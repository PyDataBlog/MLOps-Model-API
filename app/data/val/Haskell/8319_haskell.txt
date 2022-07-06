module Algebra.Structures.Group
    ( module Algebra.Structures.Monoid
    , Group(..)
    , (<->)
    ) where


import Algebra.Structures.Monoid


class Monoid a => Group a where
    neg :: a -> a


(<->) :: Group a => a -> a -> a
a <-> b = a <+> neg b

infixl 6 <->

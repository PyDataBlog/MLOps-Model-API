module Algebra.Structures.Field
    ( module Algebra.Structures.IntegralDomain
    , Field(..)
    , (</>)
    ) where


import Algebra.Structures.IntegralDomain


class IntegralDomain a => Field a where
    inv :: a -> a


(</>) :: Field a => a -> a -> a
x </> y = x <*> inv y

infixl 7 </>

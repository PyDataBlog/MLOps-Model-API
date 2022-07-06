module Problem7 ( flattenList, NestedList(..) ) where

data NestedList a = Elem a | List [NestedList a]

flattenList :: NestedList a -> [a]
flattenList (Elem a) = [a]
flattenList (List a) = concatMap flattenList a
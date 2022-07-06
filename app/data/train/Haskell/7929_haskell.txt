import Data.List
repli n x = concat [take n $ repeat x | x <- x]

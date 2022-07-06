{-
    H-99 Problems
    Copyright 2015 (c) Adrian Nwankwo (Arcaed0x)
    Problem     : 14
    Description : Duplicate the elements of a list.
    License     : MIT (See LICENSE file)
-}

copyTwice :: [a] -> [a]
copyTwice []     = []
copyTwice (x:xs) = x : x : copyTwice xs

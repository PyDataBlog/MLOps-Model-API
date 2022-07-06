-- file ch03/BadPattern.hs

{- An example of what happens when you omit the check for the [] constructor
when writing a series of patterns -}
badExample (x:xs) = x + badExample xs

{- Using a wild card to provide default behaviour for contructors we do not
care about -}
goodExample (x:xs) = x + goodExample xs
goodExample _      = 0

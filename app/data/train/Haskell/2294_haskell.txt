{- The authors ask:
2. Write a function, lastButOne , that returns the element before the last.
-}

lastButOne :: [a] -> a
lastButOne xs = last (take ((length xs) - 1) xs)

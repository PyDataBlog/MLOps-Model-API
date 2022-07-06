compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = compress' x xs
    where
        compress' x [] = [x]
        compress' y (x:xs)
            | y == x    = compress' x xs
            | otherwise = y:(compress' x xs)
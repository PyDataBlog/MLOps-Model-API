-- Run-length encoding of a list. Use the result of problem P09 to implement the
-- so-called run-length encoding data compression method. Consecutive duplicates
-- of elements are encoded as lists (N E) where N is the number of duplicates of
-- the element E.

----- From Ex09
pack :: (Eq a) => [a] -> [[a]]

computeTail :: (Eq a) => a -> [a] -> Int -> (Int, [a])
computeTail a [] n = (n, [])
computeTail a (x : xs) n = if a == x
                           then computeTail a xs (n + 1)
                           else (n, (x : xs))

pack [] = []
pack (x : xs) = (replicate n x) : (pack tailNoDup)
                where (n, tailNoDup) = computeTail x xs 1

----- Ex10

encode :: (Eq a) => [a] -> [(Int, a)]
encode l = map (\ l -> (length l, head l)) (pack l)

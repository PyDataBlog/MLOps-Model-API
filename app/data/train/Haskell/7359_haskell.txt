import Prelude

data Encoded a = Single a | Multiple Int a
    deriving Show

encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect [] = []
encodeDirect xs = foldr (append) [] xs
    where
        append y [] = [Single y]
        append y ((Single z):zs)
            | (y == z) = (Multiple 2 z):zs
            | otherwise = (Single y):(Single z):zs
        append y ((Multiple n z):zs)
            | (y == z) = (Multiple (n + 1) z):zs
            | otherwise = (Single y):(Multiple n z):zs

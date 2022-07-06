c2n :: Num a => Char -> a
c2n x = case x of
        '0' -> 0
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
main :: IO ()
main = print . sum [c2n x | x <- show (2 ^ 1000)]
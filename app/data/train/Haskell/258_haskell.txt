module Main where
comb :: [([Char], [Char])]
color = ["blue", "red", "green"]
comb = [(x, y) | x <- color, y <- color, x < y]
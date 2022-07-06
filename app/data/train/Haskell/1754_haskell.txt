-- list comprehensions

-- Applying an expression to each value in a list
ghci> [x*2 | x <- [1..10]]
[2,4,6,8,10,12,14,16,18,20]

-- And also filtering out some of the values
ghci> [x*2 | x <- [1..10], x*2 > 12 ]
[14,16,18,20]

-- Using a function to filter out some of the vaalues
ghci> [x | x <- [50..100], x `mod` 7 == 4]
[53,60,67,74,81,88,95]

-- Put the comprehension into a function, to allow reuse:
ghci> let boomBangs xs = [ if x < 3 then "BOOM!" else "BANG!" | x <- xs, odd x ]
ghci> boomBangs [1..10]
["BOOM!","BANG!","BANG!","BANG!","BANG!"]

-- Can include multiple predicates, sepaarted by commas
ghci>[ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
[10,11,12,14,16,17,18,20]

-- Can draw from more than one lists
-- In which case, every combination of elements is generated.
-- This taks first value from first list, then loops through all values from the second list
-- Then takes second vlaue from the first list.
ghci>[ x + y | x <- [1..2], y <- [ 10, 100, 1000] ]
[11,101,1001,12,102,1002]

-- Combining lists og words
ghci> let nouns = ["cat","dog","frog"]
ghci> let adjectives = ["happy","smily","grumpy"]
ghci> [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]
["happy cat","happy dog","happy frog","smily cat","smily dog","smily frog",
"grumpy cat","grumpy dog","grumpy frog"]

-- Using underscore as a temporary variable, as we don't care about the values
ghci>let length' xs = sum [ 1 | _ <- xs ]
ghci>length "hello world"
11


-- Project Euler Problem 22 - names scores
--
-- Sum of letter values in names weighted by position in list
--

-- import Data.String
import Data.List

alph = zip ['A'..'Z'] [1..26]
elim_just :: Maybe a -> a
elim_just (Just a) = a

wordscore x = sum [ elim_just (lookup a alph) | a <- x]

-- str = "\"MARY\",\"PATRICIA\",\"BOB\""

split :: (Eq c) => c -> [c] -> [[c]]
split _ [] = []
-- split d (s:d:t) = s:(split d (s:d:t))
split d x = a:(if b==[] then [] else split d (tail b))
    where (a,b) = span (/= d) x

delfromlist :: (Eq c) => c -> [c] -> [c]
delfromlist d x = [a | a<-x, a /= d]

sorted_list str = (sort (split ',' (delfromlist '\"'  str)))

para_score :: [(Int, String)] -> Int
para_score [] = 0
para_score (x:xs) = ((fst x)*(wordscore (snd x))) + (para_score xs)

main = do
    -- print (words (replace "\"" "" (replace "," " " str)))
    -- print (zip [1..] sorted_list)
    str <- readFile "p022_names.txt"
    print (para_score (zip [1..] (sorted_list str)))
	

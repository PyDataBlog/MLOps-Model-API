import System.Random
import System.IO.Unsafe
import Data.List
import Data.Char

--Problem 1: Retrieve the last element from a list
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs
--


--Problem 2: Retrieve the second to last element from a list
myButLast :: [a] -> a
myButLast xs = myLast (init xs)
--


--Problem 3: Find the kth element of a list
    -- if k > length, returns the last element
elementAt :: [a] -> Int -> a
elementAt xs k = myLast (take k xs)
--


--Problem 4: Find the number of elements of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = myLength xs + 1
--


--Problem 5: Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
--


--Problem 6: Find out whether a list is a palindrome
myPalindrome :: (Eq a) => [a] -> Bool
myPalindrome [] = True
myPalindrome [_] = True
myPalindrome xs = myPalindrome (init (tail xs)) && head xs == last xs
--


----Problem 7: Flatten a nested list structure
--data NestedList a = Elem a | List [NestedList a]
--myFlatten :: NestedList a -> [a]
--myFlatten (Elem n) = [n]


--Problem 8: Eliminate consecutive duplicates of list elements
myCompress :: (Eq a) => [a] -> [a]
myCompress [] = []
myCompress [x] = [x]
myCompress (a:b:xs) 
    | a == b = (myCompress (a:xs))
    | otherwise = a:(myCompress (b:xs))
--


--Problem 9: Pack consecutive duplicates of list elements into sublists
myPack :: (Eq a) => [a] -> [[a]]
myPack [] = []
myPack xs = 
    let numDupes = countFirstDuplicates xs
    in [take numDupes xs] ++ myPack (drop numDupes xs) 


countFirstDuplicates :: (Eq a) => [a] -> Int
countFirstDuplicates [] = 0
countFirstDuplicates [a] = 1
countFirstDuplicates (a:b:xs) 
    | a == b = countFirstDuplicates (b:xs) + 1
    | otherwise = 1
--


--Problem 10: Run-length encoding of a list
myEncode :: (Eq a) => [a] -> [(Int, a)]
myEncode [] = []
myEncode xs = 
    let packedHead = head (myPack xs)
        headLength = length packedHead
    in [(length packedHead, head packedHead)] ++ myEncode (drop headLength xs)


--Problem 11: Modified run-length encoding


--Problem 12: Decode a run-length encoded list


--Problem 13: Run-length encoding of a list (direct solution)


--Problem 14: Duplicate the elements of a list
myDupli :: [a] -> [a]
myDupli [] = []
myDupli (x:xs) = x:x:myDupli xs
--


--Problem 15: Replicate the elements of a list a given number of times
myRepli :: [a] -> Int -> [a]
myRepli _ 0 = []
myRepli [] n = []
myRepli (x:xs) n =
    x:myRepli [x] (n - 1) ++ (myRepli xs n)
--


--Problem 16: Drop every N'th element from a list
myDropEvery :: [a] -> Int -> [a]
myDropEvery [] _ = []
myDropEvery xs n
    | length xs < n = xs
    | otherwise = init (take n xs) ++ myDropEvery (drop n xs) n 
--


--Problem 17: Split a list into two parts; the length of the first part is given
    --Do not use any predefined predicates.
mySplit :: [a] -> Int -> ([a], [a])
mySplit xs n = (myTake n xs, myDrop n xs)


myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) =
    x:myTake (n - 1) xs


myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 xs = xs
myDrop n (x:xs) =
    myDrop (n - 1) xs 
--


--Problem 18: Extract a slice from a list
mySlice :: [a] -> Int -> Int -> [a]
mySlice xs i k
    | k <= i = []
    | otherwise = take (k - i + 1) (drop (i - 1) xs)
--


--Problem 19: Rotate a list N places to the left
myRotate :: [a] -> Int -> [a]
myRotate xs 0 = xs
myRotate xs n
    | n > 0 = myRotate (tail xs ++ [head xs]) (n - 1)
    | otherwise = myRotate (last xs : init xs) (n + 1)
--


--Problem 20: Remove the K'th element from a list. 
    -- counting from 1
myRemoveAt :: [a] -> Int -> (a, [a])
myRemoveAt xs n = (xs !! (n - 1), take (n - 1) xs ++ drop n xs)
--


--Problem 21: Insert an element at a given position into a list.
    -- counting from 1
    -- If the number is outside the range, insert at the proper end.
myInsertAt :: a -> [a] -> Int -> [a]
myInsertAt x xs n = take (n - 1) xs ++ [x] ++ drop (n - 1) xs 
--


--Problem 22: Create a list containing all integers within a given range.
myRange :: Int -> Int -> [Int]
myRange a b = [a..b]
--


--Problem 23: Extract a given number of randomly selected elements from a list.
myRandomSelect :: [a] -> Int -> [a]
myRandomSelect _ 0 = []
myRandomSelect [] _ = []
myRandomSelect xs n = 
    let index = myRandom 0 (length xs - 1)
    in [xs !! index] ++ myRandomSelect (snd (myRemoveAt xs (index + 1))) (n - 1)
--


--Problem 24: Lotto: Draw N different random numbers from the set 1..M.
myRandomSelectFromRange :: Int -> Int -> [Int]
myRandomSelectFromRange n maxLim = 
    myRandomSelect [1..maxLim] n


--Problem 25: Generate a random permutation of the elements of a list.
myRandomPermute :: [a] -> [a]
myRandomPermute xs = myRandomSelect xs (length xs)
--


myRandom :: Int -> Int -> Int
--Don't do this, evidently. Must figure out IO. 
myRandom a b = unsafePerformIO (randomRIO (a, b)) 



--Problem 26: Generate the combinations of K distinct objects chosen from the N elements of a list
--Problem 27: Group the elements of a set into disjoint subsets.




--Problem 28a
myLSort :: [[a]] -> [[a]]
myLSort [] = []
myLSort (x:xs) =
    let smallerThanHead = myLSort[a | a <- xs, length a <= length x]
        biggerThanHead = myLSort[a | a <- xs, length a > length x]
    in smallerThanHead ++ [x] ++ biggerThanHead
--


----Problem 28b
--myLfSort :: [[a]] -> [[a]]
--myLfSort [] = []
--myLfSort (x:xs) =
--    let smallerThanHead = myLfSort[a | a <- xs, countFrequencies (length a) (x:xs) <= countFrequencies lenx (x:xs)]
--        biggerThanHead = myLfSort[a | a <- xs, countFrequencies (length a) (x:xs) > countFrequencies lenx (x:xs)]
--    in smallerThanHead ++ [x] ++ biggerThanHead
--    where lenx = length x
----myLfSort (x:xs) =
----    let 

--countFrequencies :: Int -> [[a]] -> Int
--countFrequencies len list = 
--    length [a | a <- list, length a == len]
--


--Problem 29: DOES NOT EXIST
--Problem 30: DOES NOT EXIST


--Problem 31: Determine whether a given integer number is prime.
myIsPrime :: Int -> Bool
myIsPrime 1 = False
myIsPrime 2 = True
myIsPrime x =
    if numDivisors x == 1 then True else False

numDivisors :: Int -> Int
numDivisors x = 
    length [a | a <- [1..ceiling (sqrt (fromIntegral x))]{-, abs (a `mod` 6) == 1-}, x `mod` a == 0]
--


--Problem 32: Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
myGCD :: Int -> Int -> Int
myGCD a b = if b == 0 then a else myGCD b (a `mod` b)
--


--Problem 33: Determine whether two positive integer numbers are coprime.
myCoprime :: Int -> Int -> Bool
myCoprime a b = if myGCD a b == 1 then True else False
--


--Problem 34: Calculate Euler's totient function phi(m).
myTotient :: Int -> Int
myTotient n = length [a | a <- [2..n], myCoprime a n] + 1
--


--Problem 35: Determine the prime factors of a given positive integer. 
    --Construct a flat list containing the prime factors in ascending order.
myPrimeFactors :: Int -> [Int]
myPrimeFactors n
    | n < 0 = myPrimeFactors (abs n)
    | n < 2 = []
    | otherwise = a : myPrimeFactors (quot n a)
    -- should start the generation later
    where a = head [a | a <- generatePrimeList, n `mod` a == 0] 
--


--Problem 36: Determine the prime factors of a given positive integer.
    --Construct a list containing the prime factors and their multiplicity.
myPrimeFactorsMult :: Int -> [(Int, Int)]
myPrimeFactorsMult n = 
    [(snd a, fst a) | a <- myEncode (myPrimeFactors n)]
--


--Problem 37: Calculate Euler's totient function phi(m) (improved).
myPhi :: Int -> Int
myPhi n =
    let primeFactors = myPrimeFactorsMult n
    in product (map totientPart primeFactors)

totientPart :: (Int, Int) -> Int
totientPart (p1, m1) = (p1 - 1) * p1 ^ (m1 - 1)


--Problem 38: Compare the two methods of calculating Euler's totient function.
    {- myPhi is much faster than myTotient, especially considering that
    I hadn't even pre-generated the primelist and I have to 
    regenerate it every time. -}
--


--Problem 39: Given a range of integers by its lower and upper limit, 
    -- construct a list of all prime numbers in that range.
myPrimesRange :: Int -> Int -> [Int]
myPrimesRange minLimit maxLimit =
    dropWhile (< minLimit) (takeWhile (< maxLimit) generatePrimeList)
--


--Problem 40: Goldbach's conjecture.
myGoldbach :: Int -> (Int, Int)
myGoldbach n =
    if even n && n > 2 then
    let primes = myPrimesRange 2 (n - 1)
    in head [(a, n - a) | a <- primes, (n - a) `elem` primes]
    else error "Even numbers > 2 only!"
--


--Problem 41: Given a range of integers by its lower and upper limit, 
    --print a list of all even numbers and their Goldbach composition.
myGoldbachList :: Int -> Int -> [(Int, Int, Int)]
myGoldbachList minLimit maxLimit = 
    [(a, fst gb, snd gb) | a <- [minLim, minLim + 2..maxLimit], let gb = myGoldbach a]
    where minLim = minLimit + minLimit `mod` 2


myGoldbachList' :: Int -> Int -> Int -> [(Int, Int, Int)]
myGoldbachList' minLimit maxLimit primesOver =
    [(a, b, c) | (a, b, c) <- myGoldbachList minLimit maxLimit, b > primesOver, a > primesOver]
--


-- Helper to generate primes
generatePrimeList :: [Int]
generatePrimeList =
     2:[a | a <- [1, 3..], myIsPrime a]
--


--Problem 42: DOES NOT EXIST
--Problem 43: DOES NOT EXIST
--Problem 44: DOES NOT EXIST
--Problem 45: DOES NOT EXIST


--Problem 46: NOT FINISHED
myNot :: Bool -> Bool
myNot False = True
myNot True = False

myAnd, myOr, myNand, myNor, myXor, myImpl, myEqu :: Bool -> Bool -> Bool

myAnd True True = True
myAnd _ _ = False

myOr False False = False
myOr _ _ = True

myNand a b = myNot (myAnd a b)

myNor a b = myNot (myOr a b)

myXor True True = False
myXor False False = False
myXor _ _ = True

myImpl a b = (myNot a) `myOr` b

myEqu True True = True
myEqu False False = True
myEqu _ _ = False



--Problem 90: Eight Queens
myNQueens :: Int -> [[Int]]
--each element of each list represents
--its position in the column. Assuming n is
--The number of queens to place as well as the
--dimension of the board.
myNQueens n = [x | x <- permutations [1..n], queensSafe x]


queensSafe :: [Int] -> Bool
queensSafe [] = True
queensSafe l@(x:xs)
    | firstQueenSafe l == True = queensSafe xs
    | otherwise = False



firstQueenSafe :: [Int] -> Bool
firstQueenSafe l@(x:xs) =
    let unsafe1 = [x, x - 1 .. x - length l]
        unsafe2 = [x, x + 1 .. x + length l]
        zipped = zip (tail unsafe1) (tail l) ++ zip (tail unsafe2) (tail l)
        countMatches = length [x | x <- zipped, fst x == snd x]
    in if countMatches == 0 then True else False
--


--Problem 91: Knight's tour
--myKnightsTour :: Int -> (Int, Int) -> [(Int, Int)]
--myKnightsTour n end =
--    myKTRecursive n end (myCombineLists [1..8] [1..8]) []


--myKTRecursive :: Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
--myKTRecursive n end mustreach path =
--    | length mustreach == 0 = path
--    | -- 4 recursive calls for possiblepaths


--Problem 95: English Number Words
fullWords :: Int -> String
fullWords n
    | n < 10 = digitToString n
    | otherwise = fullWords (quot (n - lastDigit) 10) ++ "-" ++ digitToString (mod n 10)
        where lastDigit = mod n 10
              digitToString x = 
                ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !! x
--


--Problem 96: Syntax checker
identifier :: String -> Bool
identifier "" = True
identifier (x:xs) 
    | isLetter x == True = identifier' xs
    | otherwise = False


identifier' :: String -> Bool
identifier' "" = True
identifier' (x:xs)
    | isLetter x = identifier' xs
    | isDigit x = identifier' xs
    | x == '-' && length xs > 0 && (isLetter (head xs) || isDigit (head xs)) = identifier' (tail xs)
    | otherwise = False
--



myCombineLists :: [a] -> [a] -> [(a, a)]
myCombineLists a b =
    [(fst x, snd x) | x <- (cycle a) `zip` (myRepli b (length a))]





















mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

factorial :: (Integral a) => a -> a
factorial n = product [1..n]

myCountOccurences :: Eq a => a -> [a] -> Int
myCountOccurences _ [] = 0
myCountOccurences n (x:xs)
    | x == n = 1 + myCountOccurences n xs
    | otherwise = myCountOccurences n xs


mySort :: (Ord a) => [a] -> [a]
mySort [] = []
mySort (x:xs) = 
    let smallerThanHead = mySort[a | a <- xs, a <= x]
        biggerThanHead = mySort[a | a <- xs, a > x]
    in smallerThanHead ++ [x] ++ biggerThanHead


halveList :: [a] -> ([a], [a])
halveList [] = ([], [])
--len = (length a) `div` 2
halveList a = 
    let len = (length a) `div` 2
    in (take len a, drop len a)



applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x) 



sumMultiples :: Int -> Int -> Int -> Int
sumMultiples limit mult1 mult2 =
    sum [x | x <- [1..limit], x `mod` 3 == 0 || x `mod` 5 == 0]
--swapSignTwiceCalled :: Int -> Int
--swapSignTwiceCalled n
--    | even n && n > 0 = n + 1
--    | even n && n < 0 = 


main :: IO()
main = print $ length $ myNQueens 10
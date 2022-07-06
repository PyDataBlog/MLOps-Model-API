import Data.List

phi   :: Int -> Int
phi n =  product xs
	where
		xs = map (\(p, m) ->  (p - 1) * p ^ (m - 1)) (primeFactorsMult n)

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map encoder . group . primeFactors
	where
		encoder xs = (head xs, length xs)

primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n 2

primeFactors' :: Int -> Int -> [Int]
primeFactors' n p
	| isFactor n p && n' > 1 = p : primeFactors' n' p
	| not $ isFactor n p && n > 1 = primeFactors' n (p + 1)
	| otherwise = [p]
		where
			n' = n `div` p

isFactor n p = n `mod` p == 0

nextPrime :: Int -> Int
nextPrime n
	| isPrime (n + 1) = n + 1
	| otherwise = nextPrime (n + 1)

isPrime :: Int -> Bool
isPrime n = isPrime' n 2

isPrime' :: Int -> Int -> Bool
isPrime' 2 _ = True
isPrime' n k
	| (n - 1) == k = True
	| n `mod` k == 0 = False
	| otherwise = isPrime' n (k + 1)

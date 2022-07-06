isPrime :: Integer -> Bool
isPrime n
  | n == 1 = False
  | n > 1  = and [ mod n i /= 0 | i <- [2..n], i*i <= n]
  | otherwise = False


factorize :: Integer -> [Integer]
factorize n = factorize_worker n 2
  
factorize_worker :: Integer -> Integer -> [Integer]
factorize_worker n factor
  | n == 1 = []
  | mod n factor == 0 = factor:factorize_worker (div n factor) factor
  | otherwise = factorize_worker n (factor+1)

factorList :: [Integer] -> [(Integer, Integer)]
factorList x = factorList_worker x 0

  
factorList_worker :: [Integer] -> Integer -> [(Integer,Integer)]
factorList_worker [] n = []
factorList_worker [x] n = [(x, n + 1)]
factorList_worker (x1:x2:xs) n
  | x1 == x2  = factorList_worker (x2:xs) (n + 1)
  | x1 /= x2  = (x1, n + 1):factorList_worker (x2:xs) 0

main = do
  print $ isPrime 156789613
  print $ (factorList.factorize) 156789613

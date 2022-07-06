import System.Random
-- import Debug.Trace -- uncomment for tracing

removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (x:xs) = (x,xs)
removeAt n (x:xs) = (a,x:b)
    where (a,b) = removeAt (n-1) xs

rnd_select :: [a] -> Int -> IO [a]
rnd_select _  0 = return []
rnd_select [] _ = return []
rnd_select xs n = randomRIO(1, length xs) >>=
--  (\ i -> return  (trace ("random i = " ++ show i) i)) >>=
  (\ i -> return $ removeAt i xs) >>= 
  (\ (x, xs) -> (rnd_select xs (n-1) >>= (\ xs -> return $ x : xs)))

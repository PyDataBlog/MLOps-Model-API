-- chaper3.hs

safetail_1 :: [a] -> [a]
safetail_1 xs = if null xs
                then []
                else tail xs

safetail_2 :: [a] -> [a]
safetail_2 xs | null xs = []
              | otherwise = tail xs

safetail_3 :: [a] -> [a]
safetail_3 [] = []
safetail_3 (_:xs) = xs

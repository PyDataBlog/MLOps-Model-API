main = do cs <- getContents
          putStr $ numbering cs

numbering :: String -> String
numbering cs = unlines $ map format $ zipLineNumber $ lines cs

zipLineNumber :: [String] -> [(String, String)]
zipLineNumber xs = zip (map show [1..]) xs

format (s1, s2) = s1++"  "++s2



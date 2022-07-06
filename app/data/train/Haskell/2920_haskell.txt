module Problem6 ( isPalindrome ) where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome str = (reverse str) == str
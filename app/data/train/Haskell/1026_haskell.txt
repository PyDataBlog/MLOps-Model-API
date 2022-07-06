--Find out whether a list is a palindrome.
module Main where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs =  xs == reverse xs


main = do
	if isPalindrome [1,2,3]
		then putStrLn "fail"
		else putStrLn "pass"
	if isPalindrome [1,2,1]
		then putStrLn "pass"
		else putStrLn "fail"
	if isPalindrome "madamimadam"
		then putStrLn "pass"
		else putStrLn "fail"
	-- Why does this one error out???
	-- answer: because isPalindrome is defined to require as using a list of objects which derive from Eq
	--         the empty list is not a list of Eq objects
	--if isPalindrome []
	--	then putStrLn "pass"
	--	else putStrLn "fail"

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
import Data.List 
import Data.Maybe
import Data.Char

hypotenuse a b = sqrt(a^2 + b^2)


--Conditionals --

identifyCamel humps = if humps == 1 
    then "Dromeday"
    else "Bactrian"



--Recursive Function -- 
increasing :: (Ord a) => [a] -> Bool 
increasing xs = if xs == []
    then True 
    else if tail xs == []
        then True 
        else if head xs <= head(tail xs)
            then increasing (tail xs)
            else False 



--Pattern matching --
incr[] = True 
incr[x] = True 
incr(x:y:ys) = x <= y && incr (y:ys)
-- incr _ = True 


--Pattern Matching -- 
noVowels :: [Char] -> [Char]
noVowels "" = ""
noVowels (x:xs) = if x `elem` "aeiouAEIOU"
    then noVowels xs 
    else x : noVowels  xs 


watch :: Int -> [Char]
watch 7 = "7 o'clock and ... SHARKNADO!"
watch n = show n ++ " " ++ "o'clock and all's well"


addOneToSum y z = 
    let x = 1 
    in x + y + z 


--Recursion --
sumRange start end acc = if start > end
    then acc 
    else sumRange (start + 1) end (acc + start)
 

len :: [a] -> Int 
len [] = 0 
len (_:xs) = 1 + len xs

summ :: (Num a) => [a] -> a 
summ [] = 0 
summ (x:xs) = x + summ xs 


--Functional Composition --

add10 value = value + 10 
mult5 value = value * 5 
mult5AfterAdd10 value = (mult5 . add10) value 

quote str = "'" ++ str ++ "'"
findKeyError key = "Unable to find " ++ " " ++ (quote key)


firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not(null lst) then head lst else "empty"


--ADT-- Defined by two pieces of data --
-- A name for the type that will be used to represent its value 
-- A set of constructors that are used to create new values. 
-- These constructors may have arguments that will hold values of the specified types


-- 1) Government organizations which are known by their name
-- 2) Companies, for which you need to record its name, an identification number, a contact person 
--  and its position within the company hierachy 
-- 3) Individual clients, known by their name, surname and whether they want to receive further information 
-- about offers and discounts 

data Client = GovOrg String 
            | Company String Integer Person String 
            | Individual Person Bool 
            deriving Show 
     

data Person = Person String String Gender
            deriving Show 


data Users = Organization String Integer Person String 
           | User Bool 
           deriving Show 


data Gender = Male | Female | Unknown
            deriving Show 

{-Using Records to define Data types-}

data PersonR = PersonR {firstName :: String, lastName :: String} deriving Show 

data ClientR = GovOrgR {clientRName :: String}
            | CompanyR {clientRName ::String, companyId :: Integer, person :: PersonR, duty :: String}
            | IndividualR {person ::PersonR }
            deriving Show

hungai = IndividualR {person = PersonR{lastName = "Hungai", firstName="Kevin"}}
kevin  = PersonR{firstName = "Hungai", lastName="Kevin"}


--Functions using pattern matching 
greet :: ClientR -> String
greet IndividualR {person = PersonR{ firstName = fn }} = "Hi," ++ " " ++ fn 
greet CompanyR { clientRName = c } = "Hello," ++ " " ++ c 
greet GovOrgR { } = "Welcome"

{- Using named record puns -> # LANGUAGE NamedFieldPuns # -}



greet2 IndividualR {person = PersonR {firstName}} = "Hi, " ++ " " ++ firstName
greet2 CompanyR { clientRName } = "Hello," ++ " " ++ clientRName
greet2 GovOrgR { } = "Welcome"

{- Using named record puns with Record Wild Cards  -> # LANGUAGE RecordWildCards # -}


greet3 IndividualR {person=PersonR{..}} = "Hi," ++ " " ++ firstName
greet3 CompanyR {..} = "Hello," ++ " " ++ clientRName
greet3 GovOrgR { } = "Welcome"


{-nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR{firstName = initial:rest}) = let newName = (toUpper initial):rest in p {firstName = newName}
nameInCapitals p@(PersonR{firstName = " "}) = p -}


clientName :: Client -> String
clientName client = case client of 
                       GovOrg name  -> name 
                       Company name _ _ _ -> name 
                       Individual person ads -> 
                            case person of Person fName lName gender -> fName ++ " " ++ lName


companyName :: Client -> Maybe String
companyName client = case client of 
    Company name _ _ _ -> Just name
    _  -> Nothing 



fibonnaci :: Integer -> Integer
fibonnaci n = case n of 
    0 -> 0
    1 -> 1 
    _ -> fibonnaci (n-1) + fibonnaci (n-2)

sorted [] = True 
sorted [_] = True 
sorted (x:r@(y:_)) = x < y && sorted r 


--Card ADT's 

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight
            | Nine | Ten | Jack | Queen | King 
            deriving (Eq,Ord,Bounded,Enum,Show,Read)


data Suit = Spades | Hearts | Diamonds | Clubs 
            deriving (Eq,Enum,Show,Read)

data Card = Card Rank Suit deriving (Eq,Show,Read)

type Hand = Card 

--Returns True if all consucutive pairs satisfy


allPairs f [] = True
allPairs f [x] = True
allPairs f (x:y:ys) = f x y && allPairs f (y:ys)



data ConnType = TCP | UDP 
data UseProxy = NoProxy | Proxy String
data TimeOut = NoTimeOut | TimeOut Integer

data ConnOptions = ConnOptions {
    connType :: ConnType 
    ,connSpeed :: Integer
    ,connProxy :: UseProxy
    ,connCaching :: Bool 
    ,connKeepAlive :: Bool
    ,connTimeOut :: TimeOut

}













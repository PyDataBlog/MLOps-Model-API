{-
--Task 4
--power x*y =
--if x*y==0
--then 0
--else if (x*y) != 0
--then x+(x*(y-1))

--These are home tasks
--bb bmi
--| bmi <= 10 ="a"
--| bmi <= 5 = "b"
--| otherwise = bmi

slope (x1,y1) (x2,y2) = dy / dx
 where dy = y2-y1
       dx = x2 - x1

--Task 2
reci x =  1/x;

--Task 3
--abst x


--Task 4
sign x
 | x<0 = -1
 | x>0 = 1
 | x==0 = 0
 |otherwise = 0


signNum x =
 if x>0
  then 1
 else if x<0
  then -1
 else 0

--Task 5
threeDifferent x y z
 | x==y && y==z && x==z = True
 | otherwise = False

--Task 6
maxofThree x y z
 | x>y && x>z = x
 | y>x && y>z = y
 | otherwise = z

--Task 7
numString x
 | x==1 ="One"
 | x==2 ="Two"
 | x==3 ="Three"
 | x==4 ="Four"
 | x==5 ="Five"
 | otherwise = "Your input in not less than 6 "

(!)  _ True = True
(!)  True _ = True

--This is fibnanci funciton
fib:: Int  -> Int
fib 0 = 1
fib 1 = 1
fib x = fib(x-1) + fib(x-2)

charName ::Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
cahrName 'c' = "Cecil"
-}



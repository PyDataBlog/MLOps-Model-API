qs1 [] = []
qs1 (x : xs) = qs1 larger ++ [x] ++ qs1 smaller
               where smaller = [a | a <- xs, a <= x]
                     larger = [b | b <- xs, b > x]

qs2 [] = []
qs2 (x : xs) = reverse (qs2 smaller ++ [x] ++ qs2 larger)
               where smaller = [a | a <- xs, a <= x]
                     larger = [b | b <- xs, b > x]


qs4 [] = []
qs4 (x : xs) = reverse (qs4 smaller) ++ [x] ++ reverse(qs4 larger)
               where smaller = [a | a <- xs, a <= x]
                     larger = [b | b <- xs, b > x]    

qs5 [] = []
qs5 (x : xs) = qs5 larger ++ [x] ++ qs5 smaller
               where smaller = [a | a <- xs, a < x]
                     larger = [b | b <- xs, b > x || b == x]

qs6 [] = []
qs6 (x : xs) = qs6 larger ++ [x] ++ qs6 smaller
               where smaller = [a | a <- xs, a < x]
                     larger = [b | b <- xs, b > x]

                              
main = do
  let one = [1]
  let many = [4, 1, 2, 1, 7, 3, 5, 4, 5, 5, 7, 7]
  print("### qs1")
  print(qs1 one)
  print(qs1 many)
  print("### qs2")
  print(qs2 one)
  print(qs2 many)
  print("### qs4")
  print(qs4 one)
  print(qs4 many)
  print("### qs5")
  print(qs5 one)
  print(qs5 many)
  print("### qs6")
  print(qs6 one)
  print(qs6 many)
  
  

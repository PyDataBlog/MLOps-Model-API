-- A letra grega λ  ́e substitu ́ıda pela contrabarra “\”;
-- O ponto “.”  ́e substitu ́ıdo por ->.
-- Por exemplo, λx.x 2  ́e notado, em Haskell como: \x->x^2 ou, ainda, \x->x*x.

quad = \x->x*x

expr = \x->x^2+2*x+3

raiz = \x->(sqrt x)

map (f x = x*x) [1..10] -- <interactive>:1:9: parse error on input ‘=’

map (\x->x*x) [1..10]

f x = x*x

f [1..10]

fac n = if n==1 then 1 else (n*fac(n-1))

map (\x->(fac x)) [1..10]

{- Para carregar um modulo
   no interpretador ':module List'
-}

deleteBy (\x y -> y*x == 48) 6 [6,8,10,12] --fail

let p x y = if x `mod` y == 0 then True else False
let remove y list = [x | x <- list, not (p x y)]

Prelude> remove 4 [4..19]
[5,6,7,9,10,11,13,14,15,17,18,19]

[ x | x <- [1..4], y <- [x..5], (x+y) ‘mod‘ 2 == 0 ]

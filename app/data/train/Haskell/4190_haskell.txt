-- o MCD por euclides

{- empezamos con dous numeros x = 20 y = 16
 - se x/y da como resto 0 y é o MCD
 - se non x = y y = resto asi ate que o resto sexa 0 en tal caso teremos que y é o mcd
 - -}

mcd x y = if resto == 0
          then y
          else mcd y resto 
          where resto = x `mod` y 

mcdPm _ 1 = 1
mcdPm 1 _ = 1
mcdPm x y 
         | x `mod` y == 0 = y 
mcdPm x y = mcdPm y (x `mod` y)

 
--podemos usar o case of
sayAmount n = case n of
            1 -> "un"
            2 -> "dous"
            --n `mod` 2 == 0 -> "par" non podemos evaluar
            n -> "a ostia deles"

-- ou pattern matching
--dicimos que pasara se o parametro é x 
--no pattern matching non podemos facer comprovacions cando macheamos o argumento
--          _
--tamen podemos usar para o valor decoñecido un ward
sayAmountPM 1 = "un"
sayAmountPM 2 = "dous"
sayAmountPM n = "a ostia deles"


sayAmountGM n
             | n == 1 = "un"
             | n == 2 = "dous"
             | n `mod` 2 == 0 = "par"
             | otherwise = "a ostia deles" -- nun caso non contemplado
{- temos estructuras de control como if then else
 - pattern matching tampouco podemos evaluar expresions
 - guard matching é moi potente porque podemos evaluar expresions so booleanas
 - case of non podemos evaluar expresions
 -}

-- é tan potente e claro o pattern matching

isEmpty [] = True
--isEmpty aList = False
isEmpty _ = False -- como vemos co wildcard funciona igual pero non podemos usar a variable

--decimoslle que acepte unha lista e cando asignemos fara a decomposicion en head e tail
myhead (x:xs) = x
myhead [] = error "No head in a empty list"

mytail :: [a] -> [a]
mytail (x:xs) = xs
--mytail _ = error "No tail in a empity list"
mytail _ = []


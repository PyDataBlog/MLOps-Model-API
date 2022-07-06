-- file: ch03/Braces.hs

-- Using layout to determine structure
bar = let a =1
          b = 2
          c = 3
      in a + b + c

-- Using explicit structuring (hardly ever used)
foo = let { a = 1; b = 2;
        c = 3 }
      in a + b +c

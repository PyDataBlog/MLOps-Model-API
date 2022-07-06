invertir::[int]->[int]
invertir [ ] = [ ]
invertir (x:xs) = (invertir xs) ++[x]
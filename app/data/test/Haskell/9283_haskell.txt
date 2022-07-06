-- Write a function that takes a list and returns the same list in reverse

module Main (reverseList) where
  aux [] acc = acc
  aux (h:t) acc = aux t (h:acc)

  reverseList xs = aux xs []

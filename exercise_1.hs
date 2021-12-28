main =
  print (sum (takeWhile (<1000) [x | x <- [1..], mod x 3 == 0|| mod x 5 == 0]))

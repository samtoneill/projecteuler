{- standard recursive way -}
fib :: Int -> Int
fib n
  | n == 1 = 1
  | n == 2 = 1
  | n > 2 = fib (n-1) + fib (n-2)

{- using pattern matching -}
fib' :: Int -> [Int]
fib' n = fibList n 0 []

fibList :: Int -> Int -> [Int] -> [Int]
fibList n 0 _ = fibList n 2 [1,1]
fibList n k xs
  | k == n = xs
  | k > 1 = fibList n (k+1) (xs ++ [sum (take 2 (reverse xs))])

{-
  Using infinite lists
  at first, with fibs and tail fibs, we can get the 3rd element:

  fibs                        : [1, 1, ?
  tail fibs                   : [1, ?
  zipWith (+) fibs (tail fibs): [2, ?
-}

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

{- take the first n fib numbers}
fibs' :: Int -> [Integer]
fibs' n = take n (fibs)

{- take up to and not including a given number  -}
fibsTakeWhile :: Integer -> [Integer]
fibsTakeWhile n = takeWhile (<n) fibs

{- calculate the sum of the even fib numbers up to a given number k -}
main = do
  print ("Enter an upper limit for the fibonacci numbers")
  input <- getLine
  let n = read input :: Integer
  let f = fibsTakeWhile n
  let f_even = filter (\x -> mod x 2 == 0) f
  let f_even_sum = sum f_even
  print ("The sum of the fibonacci numbers up to " ++ show n ++ " is " ++ show f_even_sum)

{-
The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143 ?
-}

import Debug.Trace

{-
   https://stackoverflow.com/questions/10972429/lazy-evaluation-of-terms-in-an-infinite-list-in-haskell
   it would be nice to add a trace to track the lazy evaluation
-}

{- https://stackoverflow.com/questions/3596502/lazy-list-of-prime-numbers -}
lazyPrimes :: [Integer]
lazyPrimes = 2: 3: calcNextPrimes (tail lazyPrimes) [5, 7 .. ]
  where
    calcNextPrimes (p:ps) candidates =
      let (smallerSquareP, (_:biggerSquareP)) = span (< p * p) candidates in
      smallerSquareP ++ calcNextPrimes ps [c | c <- biggerSquareP, rem c p /= 0]


{-primeFactors = -}

primeFactors :: Integer -> [Integer]
primeFactors k = calcNextFactor lazyPrimes k []
  where calcNextFactor :: [Integer] -> Integer -> [Integer] -> [Integer]
        calcNextFactor (p:ps) 1 x = x
        calcNextFactor (p:ps) k x
          | k > 1 =
            if k `mod` p == 0 then
              calcNextFactor ps (div k p) (x ++ [p])
            else
              calcNextFactor ps k x
          | otherwise = []

main = do
  print ("Enter a positive number for prime factorisation")
  input <- getLine
  let n = read input :: Integer
  print (take 10 lazyPrimes)
  let xs = primeFactors n
  print ("The prime factors are: " ++ show xs)
  print ("The largest prime factor is " ++ show (last xs))

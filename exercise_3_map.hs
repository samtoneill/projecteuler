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


{-primeFactors using infnite list and map filter zipWith etc...

Note that this is slow because we are dividing k by all primes and then
comparing every result to check for an integer.

In recursion we do the division once per prime and store the result
-}


primeFactors :: Integer -> [Integer]
primeFactors k = map fst $ filter (\(x,y) -> floor y == ceiling y)
                $ takeWhile (\(x,y) -> y >= 1) x
             where x = zip lazyPrimes $ zipWith (/) (map fromIntegral $ cycle [k]) (map fromIntegral lazyPrimes)

main = do
  let x = primeFactors 100
  print (x)

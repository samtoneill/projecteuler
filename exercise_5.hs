{-

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

-}

import Data.List

{- https://stackoverflow.com/questions/3596502/lazy-list-of-prime-numbers -}
lazyPrimes :: [Integer]
lazyPrimes = 2: 3: calcNextPrimes (tail lazyPrimes) [5, 7 .. ]
  where
    calcNextPrimes (p:ps) candidates =
      let (smallerSquareP, (_:biggerSquareP)) = span (< p * p) candidates in
      smallerSquareP ++ calcNextPrimes ps [c | c <- biggerSquareP, rem c p /= 0]


{-primeFactors

  Note originally line 29 read:
    if k `mod` p == 0 then
      calcNextFactor ps (div k p) (x ++ [p])

  Here the prime was added to the list of prime factors and then discarded.
  This failed to account for the situation when a prime was repeated.

  e.g. 27 = 3*3*3, here the prime factor is 3, but the recursion would discard
  the 3 after the first division and then get stuck in an infinite loop.

  fix
    if k `mod` p == 0 then
      calcNextFactor (p:ps) (div k p) (x ++ [p])

    do not discard the prime until it no longer is divisible

    Note that here we are doing factorisation so we do not remove duplicates
    with nub.

-}

primeFactorisation :: Integer -> [Integer]
primeFactorisation k = calcNextFactor lazyPrimes k []
  where calcNextFactor :: [Integer] -> Integer -> [Integer] -> [Integer]
        calcNextFactor (p:ps) 1 x = x
        calcNextFactor (p:ps) k x
          | k > 1 =
            if k `mod` p == 0 then
              calcNextFactor (p:ps) (div k p) (x ++ [p])
            else
              calcNextFactor ps k x
          | otherwise = []

primeFacList :: [Integer] -> [[Integer]]
primeFacList = foldl (\acc x -> (primeFactorisation x):acc) []

intersect' :: [Integer] -> [Integer] -> [Integer]
intersect' [] _ = []
intersect' _ [] = []
intersect' xs ys = filter (\x -> x `elem` xs) ys


{- need to find the common terms, call primeFacList with [a,b] and then get lists back -}

gcd' :: Integer -> Integer -> Integer
gcd' a b = (product . nub) $ intersect' x y
  where (x:y:ys) = primeFacList [a,b]

lcm' :: Integer -> Integer -> Integer
lcm' a b = a*b `div` (gcd' a b)

main = do

  print(gcd 48 180)
  print(lcm 48 180)
  print(foldl (\acc x -> lcm' acc x) 1 [1..10])

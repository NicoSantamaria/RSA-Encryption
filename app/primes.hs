module Primes (isPrime) where

    -- checks if a given number is prime (naive approach)
    isPrime :: Integer -> Bool
    isPrime n
        | n <= 1    = False
        | n == 2    = True
        | even n    = False
        | otherwise = not $ any (divides) [3, 5..integerSqrt n]
            where 
                divides :: Integer -> Bool
                divides i = mod n i == 0
                integerSqrt :: Integer -> Integer
                integerSqrt = floor . sqrt . fromIntegral
